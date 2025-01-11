fn main() {
    #[cfg(not(target_arch = "wasm32"))]
    non_web::run();
}

#[cfg(not(target_arch = "wasm32"))]
mod non_web {
    use async_io::Timer;
    use my_ecs::prelude::*;
    use std::time::Duration;

    pub(super) fn run() {
        // Creates instance.
        let mut ecs = Ecs::default(WorkerPool::with_len(3), [3]);

        // Schedules a future using once system.
        ecs.add_once_system(|| schedule_future(register_map()))
            .unwrap();

        // Waits until all tasks are executed completely.
        while !ecs.run().schedule_all().wait_for_idle().is_completed() {}
    }

    async fn register_map() -> impl Command {
        // Assumes that we're reading map data from a file using async io function.
        Timer::after(Duration::from_millis(10)).await;

        // Now we got the data.
        let map_data = "\
            01000\n\
            01010\n\
            01010\n\
            01010\n\
            00010"
            .to_owned();

        // Declares a command using the data, then returns it.
        let f = move |mut ecs: Ecs| {
            // Registers map resource.
            let map = Map::new(&map_data);
            ecs.register_resource(map)
                .take()
                .map_err(EcsError::without_data)?;

            // Appends a system that uses the map.
            ecs.add_once_system(show_map).take()?;
            Ok(())
        };
        Ok(f)
    }

    fn show_map(rr: ResRead<Map>) {
        rr.print();
    }

    #[derive(Resource)]
    struct Map(Vec<Vec<char>>);

    impl Map {
        fn new(data: &str) -> Self {
            Self(data.lines().map(|line| line.chars().collect()).collect())
        }

        fn print(&self) {
            for row in self.0.iter() {
                for c in row.iter() {
                    print!("{c}");
                }
                println!();
            }
        }
    }
}
