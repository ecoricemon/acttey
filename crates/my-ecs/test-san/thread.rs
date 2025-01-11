use my_ecs::prelude::*;
use std::{
    future::Future,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Sender},
        Arc, Mutex,
    },
    task::{Context, Poll},
    thread::{self, JoinHandle},
};

fn main() {
    call_with_name(test_unsafefuture);
}

fn call_with_name(f: fn()) {
    print!("{} ... ", my_ecs::type_name!(test_unsafefuture));
    (f)();
    println!("finished");
}

/// Tests if polling to [`UnsafeFuture`] is safe over threads. Here's how the
/// test scenario looks like.
///
/// ```text
///     Timeline of each thread   
/// ---------------------------------
/// Main       | waker   | beholder
/// fut.poll() |         |
///            | wake()  |
///            |         | fut.poll()
/// ```
///
/// `beholder` must be able to see what `Main` have done to the `fut`. This
/// test will fail if data race occurs.
fn test_unsafefuture() {
    let state = Arc::new(Mutex::new(0));

    let (tx, rx) = mpsc::channel::<UnsafeFuture>();
    let c_state = Arc::clone(&state);

    let is_end = Arc::new(AtomicBool::new(false));
    let c_is_end = Arc::clone(&is_end);
    let main_th = thread::current();

    let beholder = thread::spawn(move || {
        // `beholder` will be woken up by `waker` thread.
        while let Ok(future) = rx.recv() {
            match unsafe { future.poll() } {
                Poll::Ready(()) => {
                    unsafe { drop(ReadyFuture::new(future)) };
                    break;
                }
                Poll::Pending => {}
            }
        }

        // All is ok, let's finish the test.
        assert_eq!(*c_state.lock().unwrap(), 2);
        c_is_end.store(true, Ordering::Relaxed);
        main_th.unpark();
    });

    let c_state = Arc::clone(&state);
    let future = async move {
        // If `beholder` cannot see future's change `Main` made, it will see
        // something that has not changed. Which means that it executes future
        // from the beginning.
        {
            // state: 0 -> 1
            let mut state = c_state.lock().unwrap();
            assert_eq!(*state, 0);
            *state += 1;
        }

        // `After` will spawn `waker` thread which will wake `beholder`.
        After::new(1).await;

        // state: 1 -> 2
        let mut state = c_state.lock().unwrap();
        assert_eq!(*state, 1);
        *state += 1;
    };
    let waker = MyWaker { tx };
    let future = UnsafeFuture::new(future, waker, |(), ()| {});

    // First polling on the future.
    let _ = unsafe { future.poll() };

    // Cleans up.
    while is_end.load(Ordering::Relaxed) {
        thread::yield_now();
    }
    beholder.join().unwrap();
}

#[derive(Clone)]
struct MyWaker {
    tx: Sender<UnsafeFuture>,
}

impl WakeSend for MyWaker {
    fn wake_send(&self, handle: UnsafeFuture) {
        self.tx.send(handle).unwrap();
    }
}

struct After {
    cnt: u32,
    waker: Option<JoinHandle<()>>,
}

impl After {
    fn new(cnt: u32) -> Self {
        Self { cnt, waker: None }
    }
}

impl Drop for After {
    fn drop(&mut self) {
        if let Some(waker) = self.waker.take() {
            waker.join().unwrap();
        }
    }
}

impl Future for After {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.cnt > 0 {
            let this = self.get_mut();
            if let Some(waker) = this.waker.take() {
                waker.join().unwrap();
            }

            this.cnt -= 1;

            // `waker` thread will wake up `beholder` by sending future.
            let w = cx.waker().clone();
            let waker = thread::spawn(move || w.wake());
            this.waker = Some(waker);

            Poll::Pending
        } else {
            Poll::Ready(())
        }
    }
}
