use std::{
    future::Future,
    marker::PhantomPinned,
    mem,
    pin::Pin,
    ptr::NonNull,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
    thread::{self, Thread},
};

pub trait WakeSend: Send + Sync + Clone + 'static {
    fn wake_send(&self, handle: UnsafeFuture);
}

impl<F> WakeSend for F
where
    F: Fn(UnsafeFuture) + Send + Sync + Clone + 'static,
{
    fn wake_send(&self, handle: UnsafeFuture) {
        (self)(handle)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct UnsafeFuture {
    /// Pointer to boxed [`FutureData`].
    /// This pointer also acts like 'data' field of [`std::task::RawWaker`].
    data: NonNull<u8>,
}

unsafe impl Send for UnsafeFuture {}

impl UnsafeFuture {
    /// Allocates and creates [`FutureData`] and returns its pointer
    /// wrppaed in this struct.
    pub fn new<F, R, W, Arg, CR>(future: F, waker: W, consume: fn(R, Arg) -> CR) -> Self
    where
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
        W: WakeSend,
    {
        let pinned = FutureData::new(future, waker, consume);

        // `FutureData` is created wrapped in `Pin<Box<T>>` because it's another type of future.
        // But this struct will manage it through its pointer, so let's unwrap it.
        //
        // # Safety: We won't move the `FutureData` in this struct.
        let data = unsafe {
            let boxed = Pin::into_inner_unchecked(pinned);
            NonNull::new_unchecked(Box::into_raw(boxed)).cast::<u8>()
        };

        Self { data }
    }

    /// Drops and deallocates linked [`FutureData`].
    ///
    /// # Safety
    ///
    /// Caller must call this method only once over cloned [`UnsafeFuture`]s.
    pub unsafe fn destroy(self) {
        // Safety
        // - `self.data` is a valid pointer to a `FutureData`.
        // - drop method will be called only once.
        let vtable = self.data.cast::<FutureVTable>().as_mut();
        (vtable.drop)(self.data)
    }

    /// # Safety
    ///
    /// Undefined behavior if conditions below are not met.
    /// - Linked [`FutureData`] must be alive, not have been dropped.
    pub unsafe fn is_ready(&self) -> bool {
        let vtable = self.data.cast::<FutureVTable>().as_mut();
        (vtable.is_ready)(self.data)
    }

    /// # Safety
    ///
    /// Undefined behavior if conditions below are not met.
    /// - Linked [`FutureData`] must be alive, not have been dropped.
    /// - Polling must occur on only one thread at a time,
    ///   and caller should synchronize over threads.
    /// - Type `R` must be the same as the one future creates.
    pub unsafe fn poll_unchecked<R>(self) -> Poll<TakenFuture<R>>
    where
        R: Send + 'static,
    {
        let vtable = self.data.cast::<FutureVTable>().as_mut();
        if (vtable.poll_unchecked)(self.data) {
            let take_output_unchecked = mem::transmute::<unsafe fn(), unsafe fn(NonNull<u8>) -> R>(
                vtable.take_output_unchecked,
            );
            let output = (take_output_unchecked)(self.data);
            Poll::Ready(TakenFuture {
                output,
                handle: self,
            })
        } else {
            Poll::Pending
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if conditions below are not met.
    /// - Linked [`FutureData`] must be alive, not have been dropped.
    /// - Polling must occur on only one thread at a time,
    ///   and caller should synchronize over threads.
    pub unsafe fn poll(self) -> Poll<()> {
        let vtable = self.data.cast::<FutureVTable>().as_mut();
        if (vtable.poll_unchecked)(self.data) {
            Poll::Ready(())
        } else {
            Poll::Pending
        }
    }

    /// # Safety
    ///
    /// Waker type `W` must be the same as the type determined on [`new`].
    ///
    /// [`new`]: Self::new
    pub unsafe fn will_wake<W>(self, other: &W) -> bool
    where
        W: WakeSend + Eq,
    {
        let waker = FutureData::<(), (), W, (), ()>::waker_ptr(self.data).as_ref();
        waker == other
    }

    /// # Safety
    ///
    /// Waker type `W` must be the same as the type determined on [`new`].
    ///
    /// [`new`]: Self::new
    pub unsafe fn set_waker<W>(self, waker: W) -> W
    where
        W: WakeSend,
    {
        let old = FutureData::<(), (), W, (), ()>::waker_ptr(self.data).as_mut();
        mem::replace(old, waker)
    }

    /// # Safety
    ///
    /// Argument types `Arg` and `CR` must be the same as the types determined
    /// on [`new`].
    ///
    /// [`new`]: Self::new
    pub unsafe fn consume<Arg, CR>(self, arg: Arg) -> CR {
        let vtable = self.data.cast::<FutureVTable>().as_mut();
        let delegate_consume = mem::transmute::<unsafe fn(), unsafe fn(NonNull<u8>, Arg) -> CR>(
            vtable.delegate_consume,
        );
        delegate_consume(self.data, arg)
    }
}

#[derive(Debug)]
pub struct TakenFuture<R> {
    output: R,
    handle: UnsafeFuture,
}

impl<R> TakenFuture<R> {
    /// Deallocates inner future and returns output of the future.
    /// Do not call [`UnsafeFuture::destroy`] for the inner future.
    ///
    /// # Safety
    ///
    /// See [`UnsafeFuture::destroy`].
    pub unsafe fn take(self) -> R {
        self.handle.destroy();
        self.output
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct ReadyFuture(UnsafeFuture);

impl ReadyFuture {
    /// # Safety
    ///
    /// `future` must be a valid ready future. Also, [`destroy`] must not be
    /// called on the `future`.
    ///
    /// [`destroy`]: UnsafeFuture::destroy
    pub unsafe fn new(future: UnsafeFuture) -> Self {
        debug_assert!(future.is_ready());
        Self(future)
    }

    /// Consumes output of the ready future and destroys the future.
    ///
    /// # Safety
    ///
    /// Argument types `Arg` and `CR` must be the same as the types determined
    /// on [`UnsafeFuture::new`].
    pub unsafe fn consume<Arg, CR>(self, arg: Arg) -> CR {
        // Safety: By owner of this struct, it's guaranteed to hold valid ready
        // future.
        unsafe { self.0.consume(arg) }
    }
}

impl Drop for ReadyFuture {
    fn drop(&mut self) {
        // Safety: By owner of this struct, it's guaranteed to hold valid ready
        // future.
        unsafe { self.0.destroy() };
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct FutureData<F, R, W, Arg, CR> {
    /// Functions that receive a pointer to this struct as first parameter.
    //
    // This field must be located at the first position of this struct, So, raw
    // pointers to this structs can be translated as `FutureVTable`s as well,
    // in turn, clients can call to various functions in vtable just using the
    // one pointer.
    vtable: FutureVTable,

    /// Waker that wakes up the polling thread, a.k.a. executor or runtime.
    //
    // This field must be located at the second position of this struct, So, raw
    // pointers to this structs can be translated as `W`s as well,
    waker: W,

    /// Future data.
    future: F,

    /// Output of the future.
    output: Option<R>,

    /// Function consuming the output with anonymous argument.
    consume: fn(R, Arg) -> CR,

    /// Atomic variable to synchronize memory over workers.
    sync: AtomicBool,

    _pin: PhantomPinned,
}

impl<F, R, W, Arg, CR> FutureData<F, R, W, Arg, CR>
where
    F: Future<Output = R> + Send + 'static,
    R: Send + 'static,
    W: WakeSend,
{
    pub fn new(future: F, waker: W, consume: fn(R, Arg) -> CR) -> Pin<Box<Self>> {
        // Erases type `R` from `take_output_unchecked`, so we can hold it.
        let take_output_unchecked = unsafe {
            mem::transmute::<unsafe fn(NonNull<u8>) -> R, unsafe fn()>(Self::take_output_unchecked)
        };

        // Erases type `Arg` and `CR` from `delegate_consume`, so we can hold
        // it.
        let delegate_consume = unsafe {
            mem::transmute::<unsafe fn(NonNull<u8>, Arg) -> CR, unsafe fn()>(Self::delegate_consume)
        };

        // See vtable functions below.
        let vtable = FutureVTable {
            is_ready: Self::is_ready,
            poll_unchecked: Self::poll_unchecked,
            drop: Self::drop,
            take_output_unchecked,
            wake_send: Self::wake_send,
            delegate_consume,
        };

        Box::pin(Self {
            vtable,
            waker,
            future,
            output: None,
            consume,
            sync: AtomicBool::new(false),
            _pin: PhantomPinned,
        })
    }

    /// * data - A pointer to [`FutureData`].
    ///
    /// # Safety
    ///
    /// - The given pointer must be a valid pointer to *pinned* [`FutureData`].
    unsafe fn is_ready(data: NonNull<u8>) -> bool {
        let this = data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut();
        this.output.is_some()
    }

    /// * data - A pointer to [`FutureData`].
    ///
    /// # Safety
    ///
    /// - The given pointer must be a valid pointer to *pinned* [`FutureData`].
    unsafe fn poll_unchecked(data: NonNull<u8>) -> bool {
        let this = data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut();

        // Synchronize memory.
        //
        // Future data, `FutureData`, and its handle, `UnsafeFuture` are desined
        // to be stole by other workers, which makes a problem in terms of
        // synchronization.
        // Imagine `A` polled on a future data and wrote something on it. `B`
        // wakes `C` up and gives future handle through `WakeSend`
        // implementation. Here's the problem. `B` and `C` may be synchronized,
        // but `A` and `C` isn't. Therefore `C` cannot see what `A` made on the
        // future data.
        // This atomic variable synchronizes memory for polling workers.
        //
        // Is spin lock without limit fine?
        // Blocking here means that poll() below results in wake-poll by another
        // worker before atomic store operation finished.
        // Therefore, we have to wait just one atomic store operation, which
        // will be finished quickly.
        while this
            .sync
            .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            std::thread::yield_now();
        }

        let pinned_future = Pin::new_unchecked(&mut this.future);

        // Creates `Context` from the given data pointer.
        let data = data.as_ptr().cast_const().cast::<()>();
        let raw_waker = RawWaker::new(data, raw_waker_vtable());
        let waker = Waker::from_raw(raw_waker);
        let mut cx = Context::from_waker(&waker);

        // Polls the future and returns true if it's ready.
        let res = if let Poll::Ready(output) = pinned_future.poll(&mut cx) {
            this.output = Some(output);
            true
        } else {
            false
        };

        // Synchronize memory.
        this.sync.store(false, Ordering::Release);

        res
    }

    /// Calls drop methods on [`FutureData`] pointed by the given data pointer,
    /// then release the memory.
    ///
    /// * data - A pointer to [`FutureData`].
    ///
    /// # Safety
    ///
    /// The given pointer must be a valid pointer to [`FutureData`].
    unsafe fn drop(data: NonNull<u8>) {
        let mut this = data.cast::<FutureData<F, R, W, Arg, CR>>();
        drop(Box::from_raw(this.as_mut()));
    }

    /// * data - A pointer to [`FutureData`].
    ///
    /// # Safety
    ///
    /// - The given pointer must be a valid pointer to [`FutureData`].
    /// - The `FutureData` must have been ready.
    unsafe fn take_output_unchecked(data: NonNull<u8>) -> R {
        let this = data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut();
        this.output.take().unwrap_unchecked()
    }

    /// * data - A pointer to [`FutureData`].
    ///
    /// # Safety
    ///
    /// The given pointer must be a valid pointer to [`FutureData`].
    unsafe fn wake_send(data: NonNull<u8>) {
        let this = data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut();
        this.waker.wake_send(UnsafeFuture { data })
    }

    unsafe fn delegate_consume(data: NonNull<u8>, arg: Arg) -> CR {
        let this = data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut();
        let output: R = this.output.take().unwrap_unchecked();
        (this.consume)(output, arg)
    }
}

impl<W> FutureData<(), (), W, (), ()> {
    // Address of waker is determined by its alignment only.
    // It doesn't depend on `F` and `R` because it is located right after
    // `FutureVTable` which has fixed size.
    unsafe fn waker_ptr(data: NonNull<u8>) -> NonNull<W> {
        let this = data.cast::<FutureData<(), (), W, (), ()>>().as_mut();
        let ptr = &mut this.waker as *mut _;
        NonNull::new_unchecked(ptr)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FutureVTable {
    /// A function pointer to [`FutureData::is_ready`].
    pub is_ready: unsafe fn(NonNull<u8>) -> bool,

    /// A function pointer to [`FutureData::poll_unchecked`].
    pub poll_unchecked: unsafe fn(NonNull<u8>) -> bool,

    /// A function pointer to [`FutureData::drop`].
    pub drop: unsafe fn(NonNull<u8>),

    /// A function pointer to [`FutureData::take_output_unchecked`].
    //
    // Since future return type is unknown here, this type erased function
    // pointer must be casted with correct type like
    // 'unsafe fn(NonNull<u8>) -> R'.
    pub take_output_unchecked: unsafe fn(),

    /// A function pointer to [`FutureData::wake_send`].
    pub wake_send: unsafe fn(NonNull<u8>),

    /// A function pointer to [`FutureData::delegate_consume`].
    //
    // Since future return type is unknown here, this type erased function
    // pointer must be casted with correct type like
    // 'unsafe fn(NonNull<u8>, Arg)'.
    pub delegate_consume: unsafe fn(),
}

fn raw_waker_vtable() -> &'static RawWakerVTable {
    /// * data - A pointer to [`FutureData`].
    unsafe fn clone(data: *const ()) -> RawWaker {
        RawWaker::new(data, raw_waker_vtable())
    }

    /// * data - A pointer to [`FutureData`].
    unsafe fn wake(data: *const ()) {
        wake_by_ref(data)
    }

    /// * data - A pointer to [`FutureData`].
    unsafe fn wake_by_ref(data: *const ()) {
        let vtable = data.cast::<FutureVTable>().as_ref().unwrap_unchecked();
        let data = NonNull::new_unchecked(data.cast::<u8>().cast_mut());
        (vtable.wake_send)(data)
    }

    /// * data - A pointer to [`FutureData`].
    //
    // This is a drop function for `std::task::RawWaker`, not for `FutureData`.
    // We're treating `UnsafeFuture` as the `RawWaker`,
    // So we don't have to do something here.
    unsafe fn drop(_data: *const ()) {}

    static VTABLE: RawWakerVTable = RawWakerVTable::new(clone, wake, wake_by_ref, drop);

    &VTABLE
}

pub fn block_on<F, R>(future: F) -> R
where
    F: Future<Output = R> + 'static,
    R: 'static,
{
    let unparked = Arc::new(AtomicBool::new(false));
    let waker = Waker {
        th: thread::current(),
        unparked: Arc::clone(&unparked),
    };

    // The future and its output won't be sent elsewhere.
    let future = DoNotSend(future);
    let future = UnsafeFuture::new(future, waker, |r, ()| r);

    loop {
        unsafe {
            match future.poll_unchecked::<DoNotSend<R>>() {
                Poll::Ready(x) => {
                    let r: DoNotSend<R> = x.take();
                    return r.0;
                }
                Poll::Pending => {
                    while !unparked.load(Ordering::Relaxed) {
                        thread::park();
                    }
                    unparked.store(false, Ordering::Relaxed);
                }
            };
        }
    }

    // === Internal structs ===

    #[derive(Clone)]
    struct Waker {
        th: Thread,
        unparked: Arc<AtomicBool>,
    }

    impl WakeSend for Waker {
        fn wake_send(&self, _handle: UnsafeFuture) {
            self.unparked.store(true, Ordering::Relaxed);
            self.th.unpark(); // Release in terms of Ordering.
        }
    }

    #[repr(transparent)]
    struct DoNotSend<T>(T);

    unsafe impl<T> Send for DoNotSend<T> {}

    impl<T: Future> Future for DoNotSend<T> {
        type Output = DoNotSend<T::Output>;

        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
            // Safety: Possible thanks to repr(transparent)
            let this: Pin<&mut T> = unsafe { std::mem::transmute(self) };
            match this.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(r) => Poll::Ready(DoNotSend(r)),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused)]
    use super::*;

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn test_block_on() {
        use async_io::Timer;
        use std::{sync::Arc, thread, time::Duration};

        let tid = Arc::new(thread::current().id());

        // Future will be run on the same thread calling `block_on`.
        let future = async move {
            let cur_tid = thread::current().id();
            assert_eq!(cur_tid, *tid);
            Timer::after(Duration::from_millis(1)).await;

            let cur_tid = thread::current().id();
            assert_eq!(cur_tid, *tid);
            Timer::after(Duration::from_millis(1)).await;

            let cur_tid = thread::current().id();
            assert_eq!(cur_tid, *tid);
            42
        };

        let res = block_on(future);
        assert_eq!(res, 42);
    }
}
