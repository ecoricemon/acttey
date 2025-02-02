#![allow(dead_code)]

use my_ecs::prelude::*;
use std::{
    hash::BuildHasher,
    hint,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    thread,
    time::Duration,
};

// === Defines number of workers ===
const NUM_WORKERS: usize = 3;

// === Defines `Component` ===
#[derive(Component, Clone, Copy)]
struct Ca(i64);

#[derive(Component, Clone, Copy)]
struct Cb(i64);

#[derive(Component, Clone, Copy)]
struct Cc(i64);

// === Defines `Entity` ===
#[derive(Entity, Clone, Copy)]
struct Ea {
    a: Ca,
}

#[derive(Entity, Clone, Copy)]
struct Eb {
    a: Ca,
    b: Cb,
}

#[derive(Entity, Clone, Copy)]
struct Ec {
    c: Cc,
}

// === Defines `Filter` ===
filter!(Fa, Target = Ca);
filter!(Fb, Target = Cb);

// === Defines `Resource` ===
#[derive(Resource)]
struct Ra(String);

#[derive(Resource)]
struct Rb(String);

// === Defines test primitives ===

fn try_register_system(pool: WorkerPool) -> WorkerPool {
    let pool = try_register_struct_system(pool);
    try_register_fn_system(pool)
}

fn try_register_struct_system(pool: WorkerPool) -> WorkerPool {
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);

    // Adds resources and entities.
    ecs.add_resource(Ra("A".to_owned())).unwrap();
    ecs.add_resource(Rb("B".to_owned())).unwrap();
    ecs.register_entity_of::<Ec>().unwrap();

    struct S;
    request!(
        Req,
        Read = Fa,
        Write = Fb,
        ResRead = Ra,
        ResWrite = Rb,
        EntWrite = Ec
    );

    impl System for S {
        type Request = Req;
        fn run(&mut self, _resp: Response<'_, Self::Request>) {}
    }

    ecs.add_system(SystemDesc::new().with_system(S)).unwrap();

    ecs.destroy().into()
}

/// Function systems can have five types of parameters, which are
/// 1. Read<Filter>
/// 2. Write<Filter>
/// 3. ResRead<Resource>
/// 4. ResWrite<Resource>
/// 5. EntWrite<Entity>
///
/// Tests if we're able to register those parameter combinations.
#[rustfmt::skip]
fn try_register_fn_system(pool: WorkerPool) -> WorkerPool {
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);

    // Adds resources and entities.
    ecs.add_resource(Ra("A".to_owned())).unwrap();
    ecs.add_resource(Rb("B".to_owned())).unwrap();
    ecs.register_entity_of::<Ec>().unwrap();

    let s = String::new();

    macro_rules! test {
        (
            $(r=$r:ident)?
            $(w=$w:ident)?
            $(rr=$rr:ident)?
            $(rw=$rw:ident)?
            $(ew=$ew:ident)?
        ) => {
            paste::paste! {
                // Registers as FnMut.
                ecs.add_system(
                    SystemDesc::new().with_system(
                        |
                            $([<_ $r>]: Read<Fa>,)?
                            $([<_ $w>]: Write<Fb>,)?
                            $([<_ $rr>]: ResRead<Ra>,)?
                            $([<_ $rw>]: ResWrite<Rb>,)?
                            $([<_ $ew>]: EntWrite<Ec>,)?
                        | {}
                    )
                )
                .unwrap();

                // Registers as FnOnce.
                let c_s = s.clone();
                ecs.add_system(
                    SystemDesc::new().with_once(
                        move |
                            $([<_ $r>]: Read<Fa>,)?
                            $([<_ $w>]: Write<Fb>,)?
                            $([<_ $rr>]: ResRead<Ra>,)?
                            $([<_ $rw>]: ResWrite<Rb>,)?
                            $([<_ $ew>]: EntWrite<Ec>,)?
                        | { drop(c_s); }
                    )
                )
                .unwrap();
            }
        };
    }

    // o, o, o, o, o
    test!();
    // R, o, o, o, o
    test!(r=r);
    // o, W, o, o, o
    test!(w=w);
    // R, W, o, o, o
    test!(r=r w=w);
    // o, o, RR, o, o
    test!(rr=rr);
    // R, o, RR, o, o
    test!(r=r rr=rr);
    // o, W, RR, o, o
    test!(w=w rr=rr);
    // R, W, RR, o, o
    test!(r=r w=w rr=rr);
    // o, o, o, RW, o
    test!(rw=rw);
    // R, o, o, RW, o
    test!(r=r rw=rw);
    // o, W, o, RW, o
    test!(w=w rw=rw);
    // R, W, o, RW, o
    test!(r=r w=w rw=rw);
    // o, o, RR, RW, o
    test!(rr=rr rw=rw);
    // R, o, RR, RW, o
    test!(r=r rr=rr rw=rw);
    // o, W, RR, RW, o
    test!(w=w rr=rr rw=rw);
    // R, W, RR, RW, o
    test!(r=r w=w rr=rr rw=rw);
    // o, o, o, o, EW
    test!(ew=ew);
    // R, o, o, o, EW
    test!(r=r ew=ew);
    // o, W, o, o, EW
    test!(w=w ew=ew);
    // R, W, o, o, EW
    test!(r=r w=w ew=ew);
    // o, o, RR, o, EW
    test!(rr=rr ew=ew);
    // R, o, RR, o, EW
    test!(r=r rr=rr ew=ew);
    // o, W, RR, o, EW
    test!(w=w rr=rr ew=ew);
    // R, W, RR, o, EW
    test!(r=r w=w rr=rr ew=ew);
    // o, o, o, RW, EW
    test!(rw=rw ew=ew);
    // R, o, o, RW, EW
    test!(r=r rw=rw ew=ew);
    // o, W, o, RW, EW
    test!(w=w rw=rw ew=ew);
    // R, W, o, RW, EW
    test!(r=r w=w rw=rw ew=ew);
    // o, o, RR, RW, EW
    test!(rr=rr rw=rw ew=ew);
    // R, o, RR, RW, EW
    test!(r=r rr=rr rw=rw ew=ew);
    // o, W, RR, RW, EW
    test!(w=w rr=rr rw=rw ew=ew);
    // R, W, RR, RW, EW
    test!(r=r w=w rr=rr rw=rw ew=ew);

    ecs.destroy().into()
}

fn try_open_close(pool: WorkerPool) -> WorkerPool {
    // Creates instance.
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);

    const REPEAT: usize = 10;

    // Opens and closes workers without doing something.
    for _ in 0..REPEAT {
        let _ = ecs.run();
    }

    ecs.destroy().into()
}

fn try_schedule(pool: WorkerPool) -> WorkerPool {
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);
    // Registers and inserts entities.
    ecs.register_entity_of::<Ea>()
        .register_entity_of::<Eb>()
        .add_system(SystemDesc::new().with_once(|ew: EntWrite<Ea>| {
            let mut ew = ew.take_recur();
            ew.add(Ea { a: Ca(1) });
            ew.add(Ea { a: Ca(2) });
        }))
        .add_system(SystemDesc::new().with_once(|ew: EntWrite<Eb>| {
            let mut ew = ew.take_recur();
            ew.add(Eb {
                a: Ca(3),
                b: Cb(10),
            });
            ew.add(Eb {
                a: Ca(4),
                b: Cb(20),
            });
        }))
        // Adds resources.
        .add_resource(Ra("A".to_owned()))
        .add_resource(Rb("B".to_owned()))
        // Test.
        .add_system(SystemDesc::new().with_system(inc_ca))
        .add_system(SystemDesc::new().with_system(inc_cb))
        .add_system(SystemDesc::new().with_system(dec_ca))
        .add_system(SystemDesc::new().with_system(dec_cb))
        .add_system(SystemDesc::new().with_system(iter_ca))
        .add_system(SystemDesc::new().with_system(iter_cb))
        .add_system(SystemDesc::new().with_system(attach_ra))
        .add_system(SystemDesc::new().with_system(detach_ra))
        .step();

    assert!(ecs.collect_poisoned_systems().is_empty());

    return ecs.destroy().into();

    // === Internal struct and functions ===

    /// Increases `Ca` by 1.
    fn inc_ca(mut w: Write<Fa>) {
        w.iter_mut().flatten().for_each(|ca| ca.0 += 1);

        let mut vals: Vec<i64> = w.iter().flatten().map(|ca| ca.0).collect();
        vals.sort_unstable();
        assert_eq!(vals, [2, 3, 4, 5]);
    }

    /// Decreases `Ca` by 1.
    fn dec_ca(mut w: Write<Fa>) {
        w.iter_mut().flatten().for_each(|ca| ca.0 -= 1);

        let mut vals: Vec<i64> = w.iter().flatten().map(|ca| ca.0).collect();
        vals.sort_unstable();
        assert_eq!(vals, [1, 2, 3, 4]);
    }

    // Iterates over `Ca`.
    fn iter_ca(r: Read<Fa>) {
        let mut vals: Vec<i64> = r.iter().flatten().map(|ca| ca.0).collect();
        vals.sort_unstable();
        assert_eq!(vals, [1, 2, 3, 4]);
    }

    /// Increases `Cb` by 1.
    fn inc_cb(mut w: Write<Fb>) {
        w.iter_mut().flatten().for_each(|cb| cb.0 += 1);

        let mut vals: Vec<i64> = w.iter().flatten().map(|cb| cb.0).collect();
        vals.sort_unstable();
        assert_eq!(vals, [11, 21]);
    }

    /// Decreases `Cb` by 1.
    fn dec_cb(mut w: Write<Fb>) {
        w.iter_mut().flatten().for_each(|cb| cb.0 -= 1);

        let mut vals: Vec<i64> = w.iter().flatten().map(|cb| cb.0).collect();
        vals.sort_unstable();
        assert_eq!(vals, [10, 20]);
    }

    // Iterates over `Cb`.
    fn iter_cb(r: Read<Fb>) {
        let mut vals: Vec<i64> = r.iter().flatten().map(|cb| cb.0).collect();
        vals.sort_unstable();
        assert_eq!(vals, [10, 20]);
    }

    // Attaches a letter to `Ra`.
    fn attach_ra(mut w: ResWrite<Ra>) {
        w.0.push('A');
        assert_eq!(w.0, "AA");
    }

    // Detaches a letter from `Ra`.
    fn detach_ra(mut w: ResWrite<Ra>) {
        w.0.pop();
        assert_eq!(w.0, "A");
    }
}

fn try_command(pool: WorkerPool) -> WorkerPool {
    use std::sync::{Arc, Mutex};

    const REPEAT: usize = 5;

    // Creates instance.
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);

    // Counts number of system execution.
    let count = Arc::new(Mutex::new(0));
    let c_count = Arc::clone(&count);

    // In system, we're appending another system to increase the count.
    ecs.add_system(SystemDesc::new().with_once(move || {
        // Command will append a system at the end of cycle.
        let cmd = move |mut ecs: Ecs| {
            ecs.add_system(SystemDesc::new().with_system(move || {
                let mut c = c_count.lock().unwrap();
                *c += 1;
            }))
            .take()?;
            Ok(())
        };
        global::schedule_command(cmd);
    }))
    .unwrap();

    // Repeats running.
    for _ in 0..REPEAT {
        ecs.step();
    }

    // Why REPEAT - 1?
    //
    // In the first run, the system was not registered yet.
    assert_eq!(*count.lock().unwrap(), REPEAT - 1);

    ecs.destroy().into()
}

fn try_parallel_task(pool: WorkerPool) -> WorkerPool {
    const START: i64 = 0;
    const END: i64 = 10_000;
    const NUM: i64 = END - START + 1;
    const SUM: i64 = (START + END) * NUM / 2;

    // Creates instance.
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);

    // Registers and inserts entities.
    ecs.register_entity_of::<Ea>().unwrap();
    ecs.add_system(SystemDesc::new().with_once(|ew: EntWrite<Ea>| {
        let mut ew = ew.take_recur();
        ew.resize(NUM as usize, Ea { a: Ca(0) });
        let mut col = ew.get_column_mut_of::<Ca>().unwrap();
        for (ca, val) in col.iter_mut().zip(START..=END) {
            ca.0 = val;
        }
    }))
    .unwrap();

    // Adds a resource.
    #[derive(Resource)]
    struct R(Vec<i64>);
    let r = R((START..=END).into_iter().collect());
    ecs.add_resource(r).unwrap();

    // Tests pure rayon iterator wrapped in into_ecs_par.
    ecs.add_system(SystemDesc::new().with_once(|rr: ResRead<R>| {
        let sum: i64 = rr.0.par_iter().into_ecs_par().sum();
        assert_eq!(sum, SUM);
    }))
    .unwrap();
    run_with_validation(&mut ecs);

    // Tests immutable parallel iterator.
    ecs.add_system(SystemDesc::new().with_once(|r: Read<Fa>| {
        let mut sum = 0_i64;
        for getter in r.iter() {
            sum += getter.par_iter().into_ecs_par().map(|ca| ca.0).sum::<i64>();
        }
        assert_eq!(sum, SUM);
    }))
    .unwrap();
    run_with_validation(&mut ecs);

    // Tests mutable parallel iterator.
    ecs.add_system(SystemDesc::new().with_once(|mut w: Write<Fa>| {
        for mut getter in w.iter_mut() {
            getter
                .par_iter_mut()
                .into_ecs_par()
                .for_each(|ca| ca.0 *= 2);
        }

        let mut sum = 0_i64;
        for getter in w.iter_mut() {
            sum += getter.par_iter().into_ecs_par().map(|ca| ca.0).sum::<i64>();
        }
        assert_eq!(sum, SUM * 2);
    }))
    .unwrap();
    run_with_validation(&mut ecs);

    return ecs.destroy().into();

    // === Internal helper functions ===

    fn run_with_validation<W, S>(ecs: &mut EcsApp<W, S>)
    where
        W: Work + 'static,
        S: BuildHasher + Default + 'static,
    {
        // Parallel task count == 0?
        global::stat::assert_eq_parallel_task_count(0);

        // Runs.
        ecs.step();
        assert!(ecs.collect_poisoned_systems().is_empty());

        // Parallel task count > 0?
        global::stat::assert_ne_parallel_task_count(0);
        global::stat::reset_parallel_task_count();
    }
}

fn try_request_lock(pool: WorkerPool) -> WorkerPool {
    let workers: Vec<Worker> = pool.into();
    let workers = try_request_lock_ok(workers);
    let workers = try_request_lock_cancelled(workers);
    workers.into()
}

fn try_request_lock_ok(workers: Vec<Worker>) -> Vec<Worker> {
    const COUNT: u64 = 1000;

    // Creates instance.
    let num_workers = workers.len();
    let mut ecs = Ecs::default(workers, [num_workers]);

    // Adds a shared resource.
    #[derive(Resource)]
    struct Counter(i32);
    let mut cnt = Counter(0);
    let desc = unsafe { ResourceDesc::new().with_ptr(&mut cnt as *mut _) };
    ecs.add_resource(desc).unwrap();

    // An atomic variable to see exclusive access to the resource.
    let is_async = Arc::new(AtomicBool::new(false));
    let c_is_async = Arc::clone(&is_async);

    // A synchronous system writing something on the resource.
    ecs.add_system(
        SystemDesc::new()
            .with_activation(COUNT, InsertPos::Back)
            .with_system(move |mut rw: ResWrite<Counter>| {
                // Because of locking, even sync tasks cannot get executed while
                // the lock is alive.
                assert!(!is_async.load(Ordering::Relaxed));
                rw.0 += 2;
            }),
    )
    .unwrap();

    // An asynchronous system locking the resource.
    ecs.add_system(SystemDesc::new().with_once(|| {
        global::schedule_future(async move {
            request!(Req, ResWrite = Counter);
            let mut guard = global::request_lock::<Req>().await?;

            // We lock the resource for a little long. During the locking, the
            // sync task cannot get access to the resource.
            c_is_async.store(true, Ordering::Relaxed);
            thread::park_timeout(Duration::from_millis(100));
            let mut dec = || guard.res_write.0 -= 1;
            for _ in 0..COUNT {
                hint::black_box(dec());
            }
            c_is_async.store(false, Ordering::Relaxed);

            Ok(())
        });
    }))
    .unwrap();

    while !ecs.step().is_completed() {
        thread::yield_now();
    }

    // Sync task has increased the counter by 2 * COUNT.
    // While async task has decreased the counter by COUNT.
    assert_eq!(cnt.0, COUNT as i32);

    ecs.destroy()
}

fn try_request_lock_cancelled(workers: Vec<Worker>) -> Vec<Worker> {
    // Creates instance.
    let num_workers = workers.len();
    let mut ecs = Ecs::default(workers, [num_workers]);

    // Adds a shared resource.
    #[derive(Resource)]
    struct Counter(i32);
    let mut cnt = Counter(0);
    let desc = unsafe { ResourceDesc::new().with_ptr(&mut cnt as *mut _) };
    ecs.add_resource(desc).unwrap();

    // A synchronous system writing something on the resource.
    ecs.add_system(
        SystemDesc::new().with_system(move |mut rw: ResWrite<Counter>| {
            rw.0 += 1;
        }),
    )
    .unwrap();

    // An asynchronous system locking the resource.
    ecs.add_system(SystemDesc::new().with_once(|| {
        global::schedule_future(async move {
            request!(Req, ResWrite = Counter);
            let mut guard = global::request_lock::<Req>().await?;
            guard.res_write.0 += 1;
            Ok(())
        });
    }))
    .unwrap();

    ecs.step();

    // `request_lock` command or system will be cancelled by destruction of ecs.
    let workers = ecs.destroy();

    // Sync system increased it by 1, while future couldn't have a chance to
    // modify the counter.
    assert_eq!(cnt.0, 1);

    workers
}

fn try_recover_from_panic(pool: WorkerPool) -> (WorkerPool, i32) {
    const START: i64 = 0;
    const END: i64 = 10;
    const NUM: i64 = END - START + 1;
    const SUM: i64 = (START + END) * NUM / 2;

    // Creates instance.
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);

    // Registers and inserts entities.
    ecs.register_entity_of::<Ea>().unwrap();
    ecs.add_system(SystemDesc::new().with_once(|ew: EntWrite<Ea>| {
        let mut ew = ew.take_recur();
        for val in START..=END {
            ew.add(Ea { a: Ca(val) });
        }
    }))
    .unwrap();

    // Adds resources.
    ecs.add_resource(Ra("".to_owned())).unwrap();

    // Adds systems.
    let ok_sys = |r: Read<Fa>, mut rw: ResWrite<Ra>| {
        rw.0.push('x');
        let sum: i64 = r.iter().flatten().map(|ca| ca.0).sum();
        assert_eq!(sum, SUM);
    };
    let fail_sys = |_w: Write<Fa>| panic!("Panics on purpose");

    let sid_ok_a = ecs
        .add_system(SystemDesc::new().with_system(ok_sys))
        .unwrap();
    let sid_fail = ecs
        .add_system(SystemDesc::new().with_system(fail_sys))
        .unwrap();
    let sid_ok_b = ecs
        .add_system(SystemDesc::new().with_system(ok_sys))
        .unwrap();

    ecs.step();
    let poisoned = ecs.collect_poisoned_systems();

    assert_eq!(poisoned.len(), 1);
    assert_eq!(sid_fail, poisoned[0].id());

    ecs.step();

    // TODO: check system len.

    assert!(ecs.inactivate_system(sid_ok_a).is_ok());
    assert!(ecs.inactivate_system(sid_ok_b).is_ok());

    ecs.add_system(SystemDesc::new().with_once(|rr: ResRead<Ra>| {
        assert_eq!(rr.0.len(), 4);
    }))
    .unwrap();

    const NUM_EXPECTED_PANICS: i32 = 1;
    (ecs.destroy().into(), NUM_EXPECTED_PANICS)
}

fn try_recover_from_panic_in_parallel_task(pool: WorkerPool) -> (WorkerPool, i32) {
    const START: i64 = 0;
    const END: i64 = 10_000;
    const NUM: i64 = END - START + 1;
    const SUM: i64 = (START + END) * NUM / 2;

    // Creates instance.
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);

    // Registers and inserts entities.
    ecs.register_entity_of::<Ea>().unwrap();
    ecs.add_system(SystemDesc::new().with_once(|ew: EntWrite<Ea>| {
        let mut ew = ew.take_recur();
        for val in START..=END {
            ew.add(Ea { a: Ca(val) });
        }
    }))
    .unwrap();

    // Adds resources.
    ecs.add_resource(Ra("".to_owned())).unwrap();

    // Adds systems.
    let ok_sys = |r: Read<Fa>, mut rw: ResWrite<Ra>| {
        rw.0.push('x');
        let sum: i64 = r.iter().flatten().map(|ca| ca.0).sum();
        assert_eq!(sum, SUM);
    };
    let fail_sys = |r: Read<Fa>| {
        let mut sum = 0_i64;
        for getter in r.iter() {
            sum += getter
                .par_iter()
                .into_ecs_par()
                .fold(
                    || 0_i64,
                    |sum, ca| {
                        if ca.0 == START + END / 2 {
                            panic!("Panic on purpose");
                        }
                        sum + ca.0
                    },
                )
                .reduce(|| 0_i64, |sum_a, sum_b| sum_a + sum_b);
        }
        assert_eq!(sum, SUM);
    };

    let sid_ok_a = ecs
        .add_system(SystemDesc::new().with_system(ok_sys))
        .unwrap();
    let sid_fail = ecs
        .add_system(SystemDesc::new().with_system(fail_sys))
        .unwrap();
    let sid_ok_b = ecs
        .add_system(SystemDesc::new().with_system(ok_sys))
        .unwrap();

    ecs.step();
    let poisoned = ecs.collect_poisoned_systems();

    assert_eq!(poisoned.len(), 1);
    assert_eq!(sid_fail, poisoned[0].id());

    ecs.step();

    // TODO: check system len.

    assert!(ecs.inactivate_system(sid_ok_a).is_ok());
    assert!(ecs.inactivate_system(sid_ok_b).is_ok());

    ecs.add_system(SystemDesc::new().with_once(|rr: ResRead<Ra>| {
        assert_eq!(rr.0.len(), 4);
    }))
    .unwrap();

    const NUM_EXPECTED_PANICS: i32 = 1;
    (ecs.destroy().into(), NUM_EXPECTED_PANICS)
}

// === Non-web tests ===

#[cfg(not(target_arch = "wasm32"))]
mod non_web_test {
    use super::*;

    #[test]
    fn test_register_system() {
        try_register_system(worker_pool());
    }

    #[test]
    fn test_open_close() {
        try_open_close(worker_pool());
    }

    #[test]
    fn test_schedule() {
        try_schedule(worker_pool());
    }

    #[test]
    fn test_parallel_task() {
        try_parallel_task(worker_pool());
    }

    #[test]
    fn test_request_lock() {
        try_request_lock(worker_pool());
    }

    #[test]
    fn test_recover_from_panic() {
        try_recover_from_panic(worker_pool());
        try_recover_from_panic_in_parallel_task(worker_pool());
    }

    #[test]
    fn test_command() {
        try_command(worker_pool());
    }

    fn worker_pool() -> WorkerPool {
        WorkerPool::with_len(NUM_WORKERS)
    }
}

// === Web tests ===

#[cfg(target_arch = "wasm32")]
mod web_test {
    use super::*;
    use js_sys::JsString;
    use std::panic;
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    pub struct Tester {
        main: Option<MainWorker>,
    }

    #[wasm_bindgen]
    impl Tester {
        #[wasm_bindgen(constructor)]
        pub fn new() -> Self {
            // Shows panic messages on browser console.
            panic::set_hook(Box::new(|info| {
                console_error_panic_hook::hook(info);
                global::web_panic_hook(info);
            }));

            // Spawns main worker and its children.
            let main = MainWorkerBuilder::new().spawn().unwrap();

            // Sends "complete" event once it received response from main worker.
            // Then JS module will destroy this struct and notify end of test
            // to playwright.
            const COMPLETE: &str = "complete";
            main.set_on_message(|data: JsValue| {
                if let Some(s) = data.dyn_ref::<JsString>() {
                    if s == COMPLETE || s == "panic" {
                        let ev = web_sys::CustomEvent::new(COMPLETE).unwrap();
                        let window = web_sys::window().unwrap();
                        window.dispatch_event(&ev).unwrap();
                        return;
                    }
                }
                panic!("unexpected response from main worker");
            });

            // Does all success tests.
            main.spawn_children(NUM_WORKERS);
            main.init_app(|pool| {
                let mut total_panics = 0;

                let pool = try_register_system(pool);
                Self::print_ok_with(&try_register_system);

                let pool = try_open_close(pool);
                Self::print_ok_with(&try_open_close);

                let pool = try_schedule(pool);
                Self::print_ok_with(&try_schedule);

                let pool = try_command(pool);
                Self::print_ok_with(&try_command);

                let pool = try_parallel_task(pool);
                Self::print_ok_with(&try_parallel_task);

                let pool = try_request_lock(pool);
                Self::print_ok_with(&try_request_lock);

                let (pool, num_panics) = try_recover_from_panic(pool);
                total_panics += num_panics;
                Self::print_ok_with(&try_recover_from_panic);

                // TODO: Test 'should panic' in web environment.
                // try_recover_from_panic_in_parallel_task();

                // Printing out a message that starts with "playwright"
                // is something like command to playwright.
                crate::log!("playwright:expectedPanics:{total_panics}");
                web_util::worker_post_message(&COMPLETE.into()).unwrap();

                Ecs::default(pool, [NUM_WORKERS])
            });

            Self { main: Some(main) }
        }

        #[wasm_bindgen]
        pub fn destroy(&mut self) {
            self.main.take();
        }

        fn print_ok_with<T: ?Sized>(_t: &T) {
            crate::log!("test {} ... ok", my_ecs::type_name!(_t));
        }
    }
}
