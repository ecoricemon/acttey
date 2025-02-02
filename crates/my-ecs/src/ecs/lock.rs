use super::{
    cmd::{self, Command, CommandObject, RawCommand},
    entry::{Ecs, EcsEntry},
    sched::ctrl::{SUB_CONTEXT, WORKER_ID},
    sys::{
        request::{Request, Response, SystemBuffer},
        system::{InsertPos, Invoke, System, SystemData, SystemDesc, SystemId, SystemState},
    },
    worker::Message,
    DynResult,
};
use crate::ds::ManagedMutPtr;
use std::{
    fmt,
    future::Future,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
    sync::Mutex,
    task::{Context, Poll, Waker},
    thread,
    time::Duration,
};
use thiserror::Error;

pub const fn request_lock<'buf, Req>() -> RequestLockFuture<'buf, Req>
where
    Req: Request,
{
    RequestLockFuture::new()
}

// NOTE: This struct references internal fields so that it must not be moved.
pub struct RequestLockFuture<'buf, Req> {
    cmd: Option<RequestLockCommand<Req>>,
    lock: Mutex<RequestLock>,
    _marker: PhantomData<&'buf Req>,
}

impl<Req> RequestLockFuture<'_, Req>
where
    Req: Request,
{
    const fn new() -> Self {
        Self {
            cmd: None,
            lock: Mutex::new(RequestLock::new()),
            _marker: PhantomData,
        }
    }
}

impl<'buf, Req: Request> Future for RequestLockFuture<'buf, Req> {
    type Output = Result<RequestLockGuard<'buf, Req>, RequestLockError>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // Safety:
        // - `self` is not moved.
        // - Future outlives command, so referencing to command is safe.
        // - Future outlives system, so referencing to system data is safe.
        unsafe {
            let this = self.get_unchecked_mut();
            let mut lock = this.lock.lock().unwrap();
            if lock.state() == RequestLockState::INIT {
                // Creates command with system.
                let waker = cx.waker().clone();
                let lock_ptr = NonNull::new_unchecked(&this.lock as *const _ as *mut _);
                let group_index = WORKER_ID.get().group_index();
                let cmd = RequestLockCommand::new(waker, lock_ptr, group_index);

                // Sets the command to this struct.
                this.cmd = Some(cmd);

                // Retrieves system pointer from the command we just made,
                // then makes system data using the pointer.
                let this_cmd = this.cmd.as_ref().unwrap_unchecked();
                let sys = this_cmd.get_system();
                let sys_ptr = sys as *const (dyn Invoke + Send);
                let sys_ptr = NonNull::new_unchecked(sys_ptr.cast_mut());
                let sdata = RequestLockSystem::<Req>::create_data(sys_ptr);

                // Sets the system data to this struct.
                lock.set_system(sdata);

                // Releases the mutex lock.
                lock.set_state_bits(RequestLockState::SCHED_CMD);
                drop(lock);

                // Creates command object using the command in this struct,
                // then sends it.
                let cmd_obj = CommandObject::Raw(RawCommand::new(this_cmd));
                cmd::schedule_command_object(cmd_obj);

                Poll::Pending
            } else if lock.state().intersects(RequestLockState::COMPLETED) {
                let sid = lock.take_system_id().unwrap();
                let buf = lock.take_system_buffer().unwrap();
                // Safety: Scheduler guarantees that we're the only one who
                // references to the memory at `buf`.
                let resp = Response::new(&mut *buf.as_ptr());

                Poll::Ready(Ok(RequestLockGuard::new(sid, resp)))
            } else if lock.state().intersects(RequestLockState::CANCELLED) {
                Poll::Ready(Err(RequestLockError::Cancelled))
            } else {
                unreachable!()
            }
        }
    }
}

impl<Req> Drop for RequestLockFuture<'_, Req> {
    fn drop(&mut self) {
        cancel_future_or_abort(&self.lock);
    }
}

// Because we passed pointer through `RawCommand`, command or system could
// access pointers inside the future. So we need to wait for them to be called
// then cancelled.
fn cancel_future_or_abort(lock: &Mutex<RequestLock>) {
    const DELAY_MS: u64 = 10;
    const LIMIT_MS: u64 = 10_000;
    const LIMIT: usize = (LIMIT_MS / DELAY_MS) as usize;

    for i in 0.. {
        let mut lock = lock.lock().unwrap();

        if lock.state() == RequestLockState::INIT
            || lock
                .state()
                .intersects(RequestLockState::COMPLETED | RequestLockState::CANCELLED)
        {
            break;
        }

        lock.set_state_bits(RequestLockState::CANCEL);
        drop(lock);

        thread::sleep(Duration::from_millis(DELAY_MS));

        // If command or system could not be executed, something went wrong.
        if i > LIMIT {
            crate::log!("something associated with `request_lock` went wrong");
            std::process::abort();
        }
    }
}

struct RequestLockCommand<Req> {
    sys: RequestLockSystem<Req>,
    lock_ptr: Option<NonNull<Mutex<RequestLock>>>,
    group_index: u16,
}

unsafe impl<Req> Send for RequestLockCommand<Req> {}

impl<Req> RequestLockCommand<Req> {
    const fn new(waker: Waker, lock_ptr: NonNull<Mutex<RequestLock>>, group_index: u16) -> Self {
        let sys = RequestLockSystem {
            waker,
            lock_ptr: Some(lock_ptr),
            _marker: PhantomData,
        };
        Self {
            sys,
            lock_ptr: Some(lock_ptr),
            group_index,
        }
    }

    const fn get_system(&self) -> &RequestLockSystem<Req> {
        &self.sys
    }
}

impl<Req: Request> Command for RequestLockCommand<Req> {
    fn command(&mut self, mut ecs: Ecs<'_>) -> DynResult<()> {
        let Some(lock_ptr) = self.lock_ptr.take() else {
            return Ok(());
        };

        // Safety: `RequestLockFuture::lock` outlives `RequestLockCommand`.
        let lock = unsafe { lock_ptr.as_ref() };
        let mut lock = lock.lock().unwrap();
        if lock.state().intersects(RequestLockState::CANCEL) {
            lock.set_state_bits(RequestLockState::CANCELLED);
            return Ok(());
        }

        // Safety: Command must be executed with a system.
        let sdata = unsafe { lock.take_system().unwrap_unchecked() };
        lock.set_state_bits(RequestLockState::SCHED_SYS);
        drop(lock);

        let desc = SystemDesc::new()
            .with_private(true)
            .with_activation(1, InsertPos::Front)
            .with_group_index(self.group_index)
            .with_data(sdata);
        ecs.add_system(desc).take()?;
        Ok(())
    }

    fn cancel(&mut self) {
        let Some(lock_ptr) = self.lock_ptr.take() else {
            return;
        };

        // Safety: `RequestLockFuture::lock` outlives `RequestLockCommand`.
        let lock = unsafe { lock_ptr.as_ref() };
        let mut lock = lock.lock().unwrap();
        lock.set_state_bits(RequestLockState::CANCELLED);
        drop(lock);
        self.sys.waker.wake_by_ref();
    }
}

struct RequestLockSystem<Req> {
    waker: Waker,
    lock_ptr: Option<NonNull<Mutex<RequestLock>>>,
    _marker: PhantomData<Req>,
}

// Safety: `lock_ptr` references to `RequestLockFuture::lock`, and the
// `RequestLockFuture::lock` outlives `RequestLockSystem`. So it's safe.
unsafe impl<Req> Send for RequestLockSystem<Req> {}

impl<Req: Request> RequestLockSystem<Req> {
    fn cancel(&mut self) {
        let Some(lock_ptr) = self.lock_ptr.take() else {
            return;
        };

        // Safety: `RequestLockFuture::lock` outlives `RequestLockCommand`.
        let lock = unsafe { lock_ptr.as_ref() };
        let mut lock = lock.lock().unwrap();
        lock.set_state_bits(RequestLockState::CANCELLED);
        drop(lock);
        self.waker.wake_by_ref();
    }
}

impl<Req: Request> System for RequestLockSystem<Req> {
    type Request = Req;

    fn run(&mut self, _resp: Response<'_, Self::Request>) {}

    fn run_private(&mut self, sid: SystemId, buf: ManagedMutPtr<SystemBuffer>) {
        let Some(lock_ptr) = self.lock_ptr.take() else {
            return;
        };

        // Safety: `RequestLockFuture::lock` outlives `RequestLockSystem`.
        let lock = unsafe { lock_ptr.as_ref() };

        // If cancelled, we need to release system buffer.
        let mut lock = lock.lock().unwrap();
        if lock.state().intersects(RequestLockState::CANCEL) {
            lock.set_state_bits(RequestLockState::CANCELLED);

            // Safety: Scheduler guarantees that we're the only one who
            // references to the `buf` memory.
            let resp = unsafe { Response::<Req>::new(&mut *buf.as_ptr()) };
            drop(RequestLockGuard::new(sid, resp));
        } else {
            // Sets `sid` and `buf` to `RequestLockFuture`.
            lock.set_output(sid, buf);
            lock.set_state_bits(RequestLockState::COMPLETED);
        }
        drop(lock);
        self.waker.wake_by_ref();
    }

    fn on_transition(&mut self, from: SystemState, to: SystemState) {
        match (from, to) {
            (SystemState::Active, SystemState::Inactive)
            | (SystemState::Active, SystemState::Dead) => self.cancel(),
            _ => {}
        }
    }
}

pub struct RequestLockGuard<'buf, Req: Request> {
    sid: SystemId,
    resp: Option<Response<'buf, Req>>,
}

impl<'buf, Req: Request> RequestLockGuard<'buf, Req> {
    const fn new(sid: SystemId, resp: Response<'buf, Req>) -> Self {
        Self {
            sid,
            resp: Some(resp),
        }
    }
}

impl<Req: Request> fmt::Debug for RequestLockGuard<'_, Req> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RequestLockGuard")
            .field("sid", &self.sid)
            .finish_non_exhaustive()
    }
}

impl<'buf, Req: Request> Deref for RequestLockGuard<'buf, Req> {
    type Target = Response<'buf, Req>;

    fn deref(&self) -> &Self::Target {
        // Safety: `self.resp` is always occupied before drop.
        unsafe { self.resp.as_ref().unwrap_unchecked() }
    }
}

impl<Req: Request> DerefMut for RequestLockGuard<'_, Req> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: `self.resp` is always occupied before drop.
        unsafe { self.resp.as_mut().unwrap_unchecked() }
    }
}

impl<Req: Request> Drop for RequestLockGuard<'_, Req> {
    fn drop(&mut self) {
        // Drops `self.resp` first for the next borrow.
        self.resp.take();

        // Sends `Fin` message to main worker in order to release resources.
        let cx = SUB_CONTEXT.get();
        assert!(!cx.is_dangling());
        // Safety: Valid pointer.
        let cx = unsafe { cx.as_ref() };
        let wid = cx.get_comm().worker_id();
        let msg = Message::Fin(wid, self.sid);
        cx.get_comm().send_message(msg);
    }
}

struct RequestLock {
    sdata: Option<SystemData>,
    sid: Option<SystemId>,
    buf: Option<ManagedMutPtr<SystemBuffer>>,
    state: RequestLockState,
}

impl RequestLock {
    const fn new() -> Self {
        Self {
            sdata: None,
            sid: None,
            buf: None,
            state: RequestLockState::INIT,
        }
    }

    const fn state(&self) -> RequestLockState {
        self.state
    }

    fn set_system(&mut self, sdata: SystemData) {
        self.sdata = Some(sdata);
    }

    fn take_system(&mut self) -> Option<SystemData> {
        self.sdata.take()
    }

    fn take_system_id(&mut self) -> Option<SystemId> {
        self.sid.take()
    }

    fn take_system_buffer(&mut self) -> Option<ManagedMutPtr<SystemBuffer>> {
        self.buf.take()
    }

    fn set_output(&mut self, sid: SystemId, buf: ManagedMutPtr<SystemBuffer>) {
        self.sid = Some(sid);
        self.buf = Some(buf);
    }

    fn set_state_bits(&mut self, state: RequestLockState) {
        self.state |= state;
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct RequestLockState(u32);

bitflags::bitflags! {
    impl RequestLockState: u32 {
        const INIT = 1;
        const SCHED_CMD = 1 << 1;
        const SCHED_SYS = 1 << 2;
        const COMPLETED = 1 << 3;
        const CANCEL = 1 << 4;
        const CANCELLED = 1 << 5;
    }
}

impl fmt::Debug for RequestLockState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut v = Vec::new();
        if self.intersects(Self::INIT) {
            v.push("INIT");
        }
        if self.intersects(Self::SCHED_CMD) {
            v.push("SCHED_CMD");
        }
        if self.intersects(Self::SCHED_SYS) {
            v.push("SCHED_SYS");
        }
        if self.intersects(Self::COMPLETED) {
            v.push("COMPLETED");
        }
        if self.intersects(Self::CANCEL) {
            v.push("CANCEL");
        }
        if self.intersects(Self::CANCELLED) {
            v.push("CANCELLED");
        }
        f.debug_tuple("RequestLockState")
            .field(&v.join(" | "))
            .finish()
    }
}

#[derive(Error, Debug)]
pub enum RequestLockError {
    #[error("cancelled")]
    Cancelled,
}
