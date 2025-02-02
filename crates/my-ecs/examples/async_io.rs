#[cfg(not(target_arch = "wasm32"))]
fn main() {
    non_web::good_example();
    non_web::bad_example();
}

#[cfg(target_arch = "wasm32")]
fn main() {}

#[cfg(not(target_arch = "wasm32"))]
mod non_web {
    use futures::{channel::oneshot, select, FutureExt};
    use my_ecs::prelude::*;
    use std::{
        thread,
        time::{Duration, Instant},
    };

    pub(super) fn good_example() {
        // Creates instance with two groups.
        let mut ecs = Ecs::default(WorkerPool::with_len(2), [1, 1]);

        let (exit_tx, exit_rx) = oneshot::channel();

        // Runs as follows.
        // - Async io tasks in group 0.
        // - Compute tasks in group 1.

        ecs.add_once_systems((
            move || global::schedule_future(async_io_server(exit_rx)),
            move || global::schedule_future(async_io_client(exit_tx)),
        ))
        .add_system(SystemDesc::new().with_group_index(1).with_once(|| {
            global::schedule_future(async_compute());
            global::schedule_future(async_compute());
        }))
        .unwrap();

        print!("[GOOD example] ");
        ecs.run();
    }

    pub(super) fn bad_example() {
        // Creates instance.
        let mut ecs = Ecs::default(WorkerPool::with_len(2), [2]);

        let (exit_tx, exit_rx) = oneshot::channel();

        // Runs as follows.
        // - Async io tasks in group 0.
        // - Compute tasks in group 0.

        ecs.add_once_systems((
            move || global::schedule_future(async_io_server(exit_rx)),
            || {
                global::schedule_future(async_compute());
                global::schedule_future(async_compute());
            },
            move || {
                global::schedule_future(async_io_client(exit_tx));
            },
        ))
        .unwrap();

        print!("[BAD example] ");
        ecs.run();
    }

    // Function that needs to respond quickly.
    async fn async_io_server(exit_rx: oneshot::Receiver<()>) -> DynResult<()> {
        let mut server = tide::new();
        server.at("/health").get(|_| async { Ok("ok") });

        select! {
            _ = server.listen("127.0.0.1:48080").fuse() => {},
            _ = exit_rx.fuse() => {},
        };
        Ok(())
    }

    // Function that hopes quick response.
    async fn async_io_client(exit_tx: oneshot::Sender<()>) -> DynResult<()> {
        let start = Instant::now();

        let body = surf::get("http://127.0.0.1:48080/health")
            .await?
            .body_string()
            .await?;
        assert_eq!(&body, "ok");

        let elapsed = start.elapsed();
        println!("GET /health : Took {elapsed:?}");

        drop(exit_tx);
        Ok(())
    }

    // Function that takes a little bit long.
    async fn async_compute() -> DynResult<()> {
        thread::sleep(Duration::from_secs(1));
        Ok(())
    }
}
