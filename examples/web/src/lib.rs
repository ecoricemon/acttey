#![cfg(target_arch = "wasm32")]
#![allow(static_mut_refs, dead_code)]

mod mandelbrot;
mod mandelbrot_gpu;
use mandelbrot::*;
use mandelbrot_gpu::*;

use acttey::prelude::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct App {
    main: Option<MainWorker>,
    is_cpu: bool,
}

#[wasm_bindgen]
impl App {
    #[wasm_bindgen(constructor)]
    pub fn new(num_workers: i32) -> Self {
        std::panic::set_hook(Box::new(|info| {
            console_error_panic_hook::hook(info);
            web_panic_hook(info);
        }));

        match num_workers {
            0 | 1 => Self::new_with_cpu(num_workers as usize),
            -1 => Self::new_with_gpu(),
            _ => panic!("App::new() allows 0, 1, or -1 only"),
        }
    }

    fn new_with_cpu(num_workers: usize) -> Self {
        let main = MainWorkerBuilder::new().spawn().unwrap();
        main.spawn_children(num_workers);
        main.init_ecs(|pool| {
            let num_workers = pool.len();
            let mut ecs = Ecs::default(pool, [num_workers]);
            ecs.add_system(SystemDesc::new().with_system(|| {
                let mut buf = BUF.lock().unwrap();
                let args = ARGS.lock().unwrap();
                calc(&mut *buf, &*args);
            }))
            .unwrap();
            ecs.into_raw()
        });

        Self {
            main: Some(main),
            is_cpu: true,
        }
    }

    fn new_with_gpu() -> Self {
        let main = MainWorkerBuilder::new().spawn().unwrap();
        main.spawn_children(1);
        main.init_ecs(|pool| {
            let num_workers = pool.len();
            Ecs::default(pool, [num_workers]).into_raw()
        });
        main.with_ecs_await(|mut ecs| async move {
            let gpu_init = cmd::GpuInit::new(None, None).await.unwrap();
            ecs.execute_commands(gpu_init)
                .add_once_system(init_gpu_resources)
                .add_systems((on_resize, submit, read))
                .unwrap();
        });

        Self {
            main: Some(main),
            is_cpu: false,
        }
    }

    #[wasm_bindgen(js_name = "setOnMessage")]
    pub fn set_onmessage(&self, f: &js_sys::Function) {
        let f = f.clone();
        if let Some(main) = self.main.as_ref() {
            main.set_onmessage(move |_| {
                f.call0(&JsValue::null()).unwrap();
            });
        }
    }

    #[wasm_bindgen(js_name = "getResult")]
    pub fn get_result(&self, dest: &mut [u8]) {
        let buf = BUF.lock().unwrap();
        assert!(dest.len() >= buf.len());
        dest[..buf.len()].copy_from_slice(&buf[..]);
    }

    #[wasm_bindgen(js_name = "calcImage")]
    pub fn calc_image(
        &mut self,
        width: u32,
        height: u32,
        x_low: f32,
        x_high: f32,
        y_low: f32,
        y_high: f32,
    ) {
        let is_cpu = self.is_cpu;
        if let Some(main) = self.main.as_mut() {
            main.with_ecs(move |mut ecs| {
                // Locking is allowed on main worker.
                {
                    let mut args = ARGS.lock().unwrap();
                    args.size = (width, height);
                    args.x_range = (x_low, x_high);
                    args.y_range = (y_low, y_high);
                    BUF.lock().unwrap().resize((width * height * 4) as usize, 0);
                }

                // Calculates.
                ecs.schedule_all();

                if is_cpu {
                    web_util::worker_post_message(&JsValue::undefined()).unwrap();
                }
            });
        }
    }

    #[wasm_bindgen]
    pub fn destroy(&mut self) {
        self.main.take();
    }
}
