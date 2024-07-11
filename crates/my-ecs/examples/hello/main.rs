use my_ecs::{default::prelude::*, ecs::prelude::*};
use std::{
    thread,
    time::{Duration, Instant},
};

fn main() {
    let mut ecs = <EcsManager>::new();

    // Spawns workers.
    let num = thread::available_parallelism().unwrap().get();
    let mut workers = (0..num)
        .map(|i| WorkerBuilder::new(format!("worker{i}")).spawn())
        .collect::<Vec<_>>();

    // Appends system that takes 1 sec.
    for _ in 0..num {
        let _ = ecs.append_system(system_1sec, NonZeroTick::MAX, false);
    }

    // Run all systems.
    println!("Starts scheduling");
    let start = Instant::now();
    ecs.schedule(&mut workers);
    let took = Instant::now() - start;
    println!("It took {num} workers {took:?} to finish {num} systems");
}

fn system_1sec() {
    println!(
        "Sleeping 1sec on thread {:?}",
        thread::current().name().unwrap()
    );
    thread::sleep(Duration::from_secs(1));
}
