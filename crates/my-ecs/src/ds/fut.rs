use std::{
    future::Future,
    marker::PhantomPinned,
    mem,
    pin::Pin,
    ptr::NonNull,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
    thread::{self, Thread},
};

/// A trait to wake a worker and send [`UnsafeFuture`] to the worker.
pub trait WakeSend: Send + Sync + Clone + 'static {
    /// Wakes associated worker and send [`UnsafeFuture`] to the worker.
    fn wake_send(&self, handle: UnsafeFuture);
}

/// A handle to a future data.
///
/// Name contains `future`, but this struct doesn't implement [`Future`] trait.
/// It provides you poll function instead. You can call poll function on a
/// handle and get the result if the `FutureData` is ready.
///
/// Plus, this is actually a type-erased pointer to a `FutureData` so that
/// owners must deal with the pointer carefully. See the example below to get a
/// feel for how to use the struct.
///
/// # Examples
///
/// ```
/// use my_ecs::ds::{WakeSend, UnsafeFuture, ReadyFuture};
/// use std::{
///     future::Future,
///     task::{Poll, Context},
///     sync::mpsc::{self, Sender},
///     pin::Pin,
/// };
///
/// #[derive(Clone)]
/// struct MyWaker(Sender<UnsafeFuture>);
///
/// impl WakeSend for MyWaker {
///     fn wake_send(&self, handle: UnsafeFuture) {
///         self.0.send(handle).unwrap();
///     }
/// }
///
/// struct MyFuture(u32);
///
/// impl Future for MyFuture {
///     type Output = u32;
///
///     fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
///         let this = self.get_mut();
///         if this.0 == 0 {
///             Poll::Ready(10)
///         } else {
///             this.0 -= 1;
///             cx.waker().wake_by_ref();
///             Poll::Pending
///         }
///     }
/// }
///
/// let (tx, rx) = mpsc::channel();
/// let fut = MyFuture(2);
/// let waker = MyWaker(tx);
/// let consume = |ret: u32, arg: u32| ret + arg;
/// let mut u_fut = UnsafeFuture::new(fut, waker, consume);
///
/// unsafe {
///     let mut pending = 0;
///     while u_fut.poll() == Poll::Pending {
///         u_fut = rx.recv().unwrap();
///         pending += 1;
///     }
///     let r_fut = ReadyFuture::new(u_fut);
///     let res: u32 = r_fut.consume(1);
///
///     assert_eq!(pending, 2);
///     assert_eq!(res, consume(10, 1));
/// }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct UnsafeFuture {
    /// Type-erased pointer to a future data.
    ///
    /// This pointer is something like 'data' field of [`RawWaker`].
    data: NonNull<u8>,
}

unsafe impl Send for UnsafeFuture {}

impl UnsafeFuture {
    /// Creates a future data in heap memory and returns its handle.
    ///
    /// # Leaks
    ///
    /// There will be memory leak if caller doesn't deallocate the future data.
    /// Future data can be deallocated by
    /// - Calling [`UnsafeFuture::destroy`].
    /// - Turning [`UnsafeFuture`] into [`ReadyFuture`] then dropping it.
    ///   `ReadyFuture` calls [`UnsafeFuture::destroy`] when it's dropped.
    ///
    /// # Examples
    ///
    /// See [`UnsafeFuture`] documentation.
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

    /// Drops and deallocates associated future data.
    ///
    /// You may need this method when you have to cancel out not ready futures.
    ///
    /// # Safety
    ///
    /// This method must be called only once for the same handles.
    pub unsafe fn destroy(self) {
        // Safety
        // - `self.data` is a valid pointer to a `FutureData`.
        // - drop method will be called only once.
        unsafe {
            let vtable = self.data.cast::<FutureVTable>().as_mut();
            (vtable.drop)(self.data)
        }
    }

    /// Returns true if associated future data is ready.
    ///
    /// # Safety
    ///
    /// Undefined behavior if associated `FutureData` has been dropped.
    pub unsafe fn is_ready(&self) -> bool {
        unsafe {
            let vtable = self.data.cast::<FutureVTable>().as_mut();
            (vtable.is_ready)(self.data)
        }
    }

    /// Tries to make more progress on the associated future data.
    ///
    /// Returning value [`Poll::Ready`] means the `FutureData` is completely
    /// resolved and ready to provide its output. [`Poll::Pending`], on the
    /// other hand, means the `FutureData` is not yet ready and will wake async
    /// runtime via the waker you inserted at [`UnsafeFuture::new`] when it can
    /// make more progress.
    ///
    /// # Safety
    ///
    /// Associated future data must be alive, not have been dropped.
    ///
    /// # Examples
    ///
    /// See [`UnsafeFuture`] documentation.
    pub unsafe fn poll(self) -> Poll<()> {
        unsafe {
            let vtable = self.data.cast::<FutureVTable>().as_mut();
            if (vtable.poll_unchecked)(self.data) {
                Poll::Ready(())
            } else {
                Poll::Pending
            }
        }
    }

    /// Returns true if the given waker is the same as the type you inserted at
    /// [`UnsafeFuture::new`].
    ///
    /// # Safety
    ///
    /// Waker type `W` must be the same as the type you inserted at
    /// [`UnsafeFuture::new`].
    pub unsafe fn will_wake<W>(self, other: &W) -> bool
    where
        W: WakeSend + PartialEq,
    {
        unsafe {
            let waker_ptr = FutureData::<(), (), W, (), ()>::waker_ptr(self.data);
            waker_ptr.as_ref() == other
        }
    }

    /// Sets a new waker to the associated future data.
    ///
    /// # Safety
    ///
    /// Waker type `W` must be the same as the type you inserted at
    /// [`UnsafeFuture::new`].
    pub unsafe fn set_waker<W>(self, waker: W) -> W
    where
        W: WakeSend,
    {
        let old = unsafe { FutureData::<(), (), W, (), ()>::waker_ptr(self.data).as_mut() };
        mem::replace(old, waker)
    }

    /// # Safety
    ///
    /// Argument types `Arg` and `CR` must be the same as the types determined
    /// on [`UnsafeFuture::new`].
    unsafe fn consume<Arg, CR>(self, arg: Arg) -> CR {
        unsafe {
            let vtable = self.data.cast::<FutureVTable>().as_mut();
            let delegate_consume = mem::transmute::<unsafe fn(), unsafe fn(NonNull<u8>, Arg) -> CR>(
                vtable.delegate_consume,
            );
            delegate_consume(self.data, arg)
        }
    }
}

/// A handle to a *ready* future data.
///
/// The struct can be created from ready [`UnsafeFuture`] only, and it doesn't
/// provide methods such as poll except [`ReadyFuture::consume`]. You can get
/// the result from the ready `FutureData` through the consume method, then
/// associated `FutureData` will be dropped and deallocated.
///
/// See [`UnsafeFuture`] documentation to see how this struct is used.
#[derive(Debug)]
#[repr(transparent)]
pub struct ReadyFuture(UnsafeFuture);

impl ReadyFuture {
    /// Creates a new [`ReadyFuture`] from the given ready [`UnsafeFuture`].
    ///
    /// # Panics
    ///
    /// Panics if associated future data is not ready.
    ///
    /// # Safety
    ///
    /// Undefined behavior if associated `FutureData` is not alive.
    ///
    /// # Examples
    ///
    /// See [`UnsafeFuture`] documentation.
    pub unsafe fn new(future: UnsafeFuture) -> Self {
        assert!(unsafe { future.is_ready() });

        Self(future)
    }

    /// Takes the result out of associated future data, then converts it by
    /// the consume function registered at [`UnsafeFuture::new`], and then
    /// returns the converted result.
    ///
    /// By taking `self`, it's dropped at the end of the method, then drops and
    /// deallocates the associated future data as well.
    ///
    /// # Safety
    ///
    /// `Arg` and `CR` must be the same as the types determined on
    /// [`UnsafeFuture::new`].
    ///
    /// # Examples
    ///
    /// See [`UnsafeFuture`] documentation.
    pub unsafe fn consume<Arg, CR>(self, arg: Arg) -> CR {
        unsafe { self.0.consume(arg) }
        // `self` goes out of scope then be dropped.
    }
}

impl Drop for ReadyFuture {
    fn drop(&mut self) {
        unsafe { self.0.destroy() };
    }
}

#[derive(Debug)]
#[repr(C)]
struct FutureData<F, R, W, Arg, CR> {
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
    fn new(future: F, waker: W, consume: fn(R, Arg) -> CR) -> Pin<Box<Self>> {
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
        let this = unsafe { data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut() };
        this.output.is_some()
    }

    /// * data - A pointer to [`FutureData`].
    ///
    /// # Safety
    ///
    /// - The given pointer must be a valid pointer to *pinned* [`FutureData`].
    unsafe fn poll_unchecked(data: NonNull<u8>) -> bool {
        let this = unsafe { data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut() };

        // Synchronize memory.
        //
        // Future data, `FutureData`, and its handle, `UnsafeFuture` are designed
        // to be stolen by other workers, which makes a problem in terms of
        // synchronization.
        // Imagine `A` polled on a future data and wrote something on it. `B`
        // wakes `C` up and gives future handle through `WakeSend`
        // implementation. Here's the problem. `B` and `C` may be synchronized,
        // but `A` and `C` isn't. Therefore, `C` cannot see what `A` made on the
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
            thread::yield_now();
        }

        let pinned_future = unsafe { Pin::new_unchecked(&mut this.future) };

        // Creates `Context` from the given data pointer.
        let data = data.as_ptr().cast_const().cast::<()>();
        let raw_waker = RawWaker::new(data, raw_waker_vtable());
        let waker = unsafe { Waker::from_raw(raw_waker) };
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
        unsafe {
            let this = data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut();
            drop(Box::from_raw(this));
        };
    }

    /// * data - A pointer to [`FutureData`].
    ///
    /// # Safety
    ///
    /// The given pointer must be a valid pointer to [`FutureData`].
    unsafe fn wake_send(data: NonNull<u8>) {
        let this = unsafe { data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut() };
        this.waker.wake_send(UnsafeFuture { data })
    }

    unsafe fn delegate_consume(data: NonNull<u8>, arg: Arg) -> CR {
        unsafe {
            let this = data.cast::<FutureData<F, R, W, Arg, CR>>().as_mut();
            let output: R = this.output.take().unwrap_unchecked();
            (this.consume)(output, arg)
        }
    }
}

impl<W> FutureData<(), (), W, (), ()> {
    // Address of waker is determined by its alignment only.
    // It doesn't depend on `F` and `R` because it is located right after
    // `FutureVTable` which has fixed size.
    unsafe fn waker_ptr(data: NonNull<u8>) -> NonNull<W> {
        unsafe {
            let this = data.cast::<FutureData<(), (), W, (), ()>>().as_mut();
            let ptr = &mut this.waker as *mut W;
            NonNull::new_unchecked(ptr)
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct FutureVTable {
    /// A function pointer to [`FutureData::is_ready`].
    is_ready: unsafe fn(NonNull<u8>) -> bool,

    /// A function pointer to [`FutureData::poll_unchecked`].
    poll_unchecked: unsafe fn(NonNull<u8>) -> bool,

    /// A function pointer to [`FutureData::drop`].
    drop: unsafe fn(NonNull<u8>),

    /// A function pointer to [`FutureData::wake_send`].
    wake_send: unsafe fn(NonNull<u8>),

    /// A function pointer to [`FutureData::delegate_consume`].
    //
    // Since future return type is unknown here, this type erased function
    // pointer must be cast with correct type like
    // 'unsafe fn(NonNull<u8>, Arg)'.
    delegate_consume: unsafe fn(),
}

fn raw_waker_vtable() -> &'static RawWakerVTable {
    /// * data - A pointer to [`FutureData`].
    unsafe fn clone(data: *const ()) -> RawWaker {
        RawWaker::new(data, raw_waker_vtable())
    }

    /// * data - A pointer to [`FutureData`].
    unsafe fn wake(data: *const ()) {
        unsafe { wake_by_ref(data) }
    }

    /// * data - A pointer to [`FutureData`].
    unsafe fn wake_by_ref(data: *const ()) {
        unsafe {
            let vtable = data.cast::<FutureVTable>().as_ref().unwrap_unchecked();
            let data = NonNull::new_unchecked(data.cast::<u8>().cast_mut());
            (vtable.wake_send)(data)
        }
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

/// Runs the given future to completion on the current worker.
///
/// This blocks until the given future is complete, then returns the result of
/// the future.
///
/// # Examples
///
/// ```
/// use my_ecs::ds::block_on;
/// use std::{
///     future::Future,
///     task::{Poll, Context},
///     pin::Pin,
/// };
///
/// struct MyFuture {
///     count: u32,
///     result: u32,
/// }
///
/// impl Future for MyFuture {
///     type Output = u32;
///
///     fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
///         let this = self.get_mut();
///         if this.count == 0 {
///             Poll::Ready(this.result)
///         } else {
///             this.count -= 1;
///             this.result += 1;
///             cx.waker().wake_by_ref();
///             Poll::Pending
///         }
///     }
/// }
///
/// let res = block_on(MyFuture { count: 2, result: 0 });
/// assert_eq!(res, 2);
/// ```
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
            match future.poll() {
                Poll::Ready(()) => {
                    let ready_future = ReadyFuture::new(future);
                    let ret: DoNotSend<R> = ready_future.consume(());
                    return ret.0;
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
            let this: Pin<&mut T> = unsafe { mem::transmute(self) };
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
