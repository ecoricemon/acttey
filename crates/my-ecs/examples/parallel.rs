//! This example shows how to do paralell computation in ecs.

use my_ecs::prelude::*;
use std::time::Instant;

// Declares Entity, Component, and Filter.
#[derive(Entity, Clone, Copy)]
struct Ea {
    a: Ca,
}
#[derive(Component, Clone, Copy)]
struct Ca(i64);
filter!(Fa, Target = Ca);

fn main() {
    const START: i64 = 0;
    const END: i64 = 10_000_000;
    const NUM: i64 = END - START + 1;
    const SUM: i64 = (START + END) * NUM / 2;

    let pool = WorkerPool::with_all_cpus();
    let num_workers = pool.len();
    let mut ecs = Ecs::default(pool, [num_workers]);

    ecs.register_entity_of::<Ea>()
        // Puts in some numbers.
        .add_once_system(|ew: EntWrite<Ea>| {
            let mut ew = ew.take_recur();
            ew.resize(NUM as usize, Ea { a: Ca(0) });
            let mut col = ew.get_column_mut_of::<Ca>().unwrap();
            for (ca, val) in col.iter_mut().zip(START..=END) {
                ca.0 = val;
            }
        })
        // Computes in parallel.
        .add_once_system(move |r: Read<Fa>| {
            let start = Instant::now();

            // Computes sum using rayon's parallel iterator. Visit this link to see
            // what rayon is. https://github.com/rayon-rs/rayon
            let mut sum = 0_i64;
            for getter in r.iter() {
                sum += getter.par_iter().into_ecs_par().map(|ca| ca.0).sum::<i64>();
            }
            assert_eq!(sum, SUM);

            println!(
                "Summation took {:?} with parallel iterator on {num_workers} workers.",
                start.elapsed()
            );
        })
        .step();

    // For the sake of comparison, computes in sequential as well.
    ecs.add_once_system(|r: Read<Fa>| {
        let start = Instant::now();

        // Computes sum using rust sequential iterator.
        let sum: i64 = r.iter().flatten().map(|ca| ca.0).sum();
        assert_eq!(sum, SUM);

        println!(
            "Summation took {:?} with sequential iterator.",
            start.elapsed()
        );
    })
    .step();
}
