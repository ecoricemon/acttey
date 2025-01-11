use super::par::FnContext;
use crate::{
    ds::prelude::*,
    ecs::sys::{
        request::SystemBuffer,
        system::{Invoke, SystemId},
    },
    impl_from_for_enum,
};
use std::{
    any::Any,
    cell::UnsafeCell,
    ptr::NonNull,
    sync::atomic::{self, AtomicI32, Ordering},
};

#[derive(Debug)]
pub(super) enum Task {
    System(SysTask),
    Parallel(ParTask),
    Future(UnsafeFuture),
}

impl_from_for_enum!("outer" = Task; "var" = System; "inner" = SysTask);
impl_from_for_enum!("outer" = Task; "var" = Parallel; "inner" = ParTask);
impl_from_for_enum!("outer" = Task; "var" = Future; "inner" = UnsafeFuture);

impl Task {
    pub(super) const fn id(&self) -> TaskId {
        match self {
            Self::System(task) => TaskId::System(task.sid),
            Self::Parallel(task) => TaskId::Parallel(*task),
            Self::Future(task) => TaskId::Future(*task),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum TaskId {
    System(SystemId),
    Parallel(ParTask),
    Future(UnsafeFuture),
}

impl_from_for_enum!("outer" = TaskId; "var" = System; "inner" = SystemId);
impl_from_for_enum!("outer" = TaskId; "var" = Parallel; "inner" = ParTask);
impl_from_for_enum!("outer" = TaskId; "var" = Future; "inner" = UnsafeFuture);

#[derive(Debug)]
pub(super) struct SysTask {
    invoker: ManagedMutPtr<dyn Invoke + Send>,
    buf: ManagedMutPtr<SystemBuffer>,
    sid: SystemId,
}

impl SysTask {
    pub(super) const fn new(
        invoker: ManagedMutPtr<dyn Invoke + Send>,
        buf: ManagedMutPtr<SystemBuffer>,
        sid: SystemId,
    ) -> Self {
        Self { invoker, buf, sid }
    }

    pub(super) const fn sid(&self) -> SystemId {
        self.sid
    }

    pub(super) fn execute(self) -> Result<(), Box<dyn Any + Send>> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let Self {
                mut invoker,
                mut buf,
                ..
            } = self;
            let executor = std::panic::AssertUnwindSafe(move || {
                invoker.invoke(&mut buf);
            });
            std::panic::catch_unwind(executor)
        }

        #[cfg(target_arch = "wasm32")]
        {
            #[cfg(not(feature = "check"))]
            {
                let Self {
                    mut invoker,
                    mut buf,
                    ..
                } = self;
                invoker.invoke(&mut buf);
            }

            // In web and debug mode, drops `ManagedMutPtr` first
            // to be recovered when the invoker panics.
            #[cfg(feature = "check")]
            {
                let Self { invoker, buf, .. } = self;

                let mut invoker_ptr = invoker.inner();
                let mut buf_ptr = buf.inner();
                drop(invoker);
                drop(buf);
                // Safety: Managed pointers.
                unsafe { invoker_ptr.as_mut().invoke(buf_ptr.as_mut()) };
            }

            Ok(())
        }
    }
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/rayon-core/src/job.rs#L33
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ParTask {
    data: NonNull<u8>,
    f: unsafe fn(NonNull<u8>, FnContext),
}

unsafe impl Send for ParTask {}

impl ParTask {
    /// # Safety
    ///
    /// Given reference must live until this struct is dropped.
    pub(super) unsafe fn new<F, R>(t: &ParTaskHolder<F, R>) -> Self
    where
        F: FnOnce(FnContext) -> R + Send,
    {
        // Safety: Infallible.
        let data = unsafe {
            let ptr = t as *const _ as *const u8;
            NonNull::new_unchecked(ptr.cast_mut())
        };
        let f = ParTaskHolder::<F, R>::execute;
        Self { data, f }
    }

    pub(super) fn execute(self, f_cx: FnContext) {
        // Safety: Guaranteed by owner who called new().
        unsafe { (self.f)(self.data, f_cx) };
    }
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/rayon-core/src/job.rs#L72
#[derive(Debug)]
pub(super) struct ParTaskHolder<F, R> {
    f: UnsafeCell<Option<F>>,
    res: UnsafeCell<Option<R>>,
    panic: UnsafeCell<Option<Box<dyn Any + Send>>>,
    flag: ParTaskFlag,
}

impl<F, R> ParTaskHolder<F, R>
where
    F: FnOnce(FnContext) -> R + Send,
{
    pub(super) fn new(f: F) -> Self {
        Self {
            f: UnsafeCell::new(Some(f)),
            res: UnsafeCell::new(None),
            panic: UnsafeCell::new(None),
            flag: ParTaskFlag::init(),
        }
    }

    /// Converts `data` into self,
    /// Then takes function out from the function slot and executes it,
    /// and then puts the returned value in the return slot.
    unsafe fn execute(data: NonNull<u8>, f_cx: FnContext) {
        let this: &Self = data.cast().as_ref();
        let f = (*this.f.get()).take().unwrap_unchecked();

        #[cfg(not(target_arch = "wasm32"))]
        {
            let executor = std::panic::AssertUnwindSafe(move || f(f_cx));
            match std::panic::catch_unwind(executor) {
                Ok(res) => *this.res.get() = Some(res),
                Err(payload) => *this.panic.get() = Some(payload),
            }
            this.flag.done();
        }

        #[cfg(target_arch = "wasm32")]
        {
            let res = f(f_cx);
            *this.res.get() = Some(res);
            this.flag.done();
        }
    }

    pub(super) fn is_executed(&self) -> bool {
        self.flag.is_done()
    }

    /// # Safety
    ///
    /// Must have been executed.
    pub(super) unsafe fn return_or_panic_unchecked(self) -> Result<R, Box<dyn Any + Send>> {
        if let Some(res) = self.res.into_inner() {
            Ok(res)
        } else {
            let payload = self.panic.into_inner().unwrap_unchecked();
            Err(payload)
        }
    }
}

#[derive(Debug)]
#[repr(transparent)]
struct ParTaskFlag(AtomicI32);

impl ParTaskFlag {
    const INIT_INNER: i32 = 0;
    const DONE_INNER: i32 = 1;

    fn init() -> Self {
        Self(AtomicI32::new(Self::INIT_INNER))
    }

    fn done(&self) {
        self.0.store(Self::DONE_INNER, Ordering::Release);
    }

    fn is_done(&self) -> bool {
        if self.0.load(Ordering::Relaxed) == Self::DONE_INNER {
            atomic::fence(Ordering::Acquire);
            true
        } else {
            false
        }
    }
}
