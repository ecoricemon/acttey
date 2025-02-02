#![cfg(target_arch = "wasm32")]
#![allow(static_mut_refs, dead_code)]

mod mandelbrot_cpu;
mod mandelbrot_gpu;
mod share;
use mandelbrot_cpu::*;
use mandelbrot_gpu::*;
use share::*;

use acttey::prelude::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct App {
    cpu_main: Option<MainWorker>,
    gpu_main: Option<MainWorker>,
}

#[wasm_bindgen]
impl App {
    #[wasm_bindgen(constructor)]
    pub fn new(ty: &str) -> Self {
        std::panic::set_hook(Box::new(|info| {
            console_error_panic_hook::hook(info);
            global::web_panic_hook(info);
        }));

        clear_global();

        let (cpu_main, gpu_main) = match ty {
            "btnCpu" => (
                Some(Self::init_cpu_worker(web_util::available_parallelism())),
                None,
            ),
            "btnGpu" => (None, Some(Self::init_gpu_worker())),
            "btnCpuGpu" => (
                Some(Self::init_cpu_worker(
                    web_util::available_parallelism().saturating_sub(1),
                )),
                Some(Self::init_gpu_worker()),
            ),
            _ => panic!(),
        };

        Self { cpu_main, gpu_main }
    }

    fn init_cpu_worker(num_workers: usize) -> MainWorker {
        let main = MainWorkerBuilder::new()
            .with_name("cpu-main")
            .spawn()
            .unwrap();
        main.spawn_children(num_workers);
        main.init_app(|pool| {
            let num_workers = pool.len();
            let mut ecs = Ecs::default(pool, [num_workers]);
            ecs.add_system(SystemDesc::new().with_system(|| {
                let mut slot = cpu_slot();
                let args = slot.args;
                calc(&mut slot.buf, args);
            }))
            .unwrap();
            ecs
        });
        main
    }

    fn init_gpu_worker() -> MainWorker {
        let main = MainWorkerBuilder::new()
            .with_name("gpu-main")
            .spawn()
            .unwrap();
        main.init_app(|pool| {
            let num_workers = pool.len();
            Ecs::default(pool, [num_workers])
        });
        main.with_app_await(|mut ecs| async move {
            let gpu_init = cmd::GpuInit::new(None, None).await.unwrap();
            ecs.execute_commands(gpu_init)
                .add_once_system(init_gpu_resources)
                .add_systems((submit, read))
                .unwrap();
        });
        main
    }

    #[wasm_bindgen(js_name = "setOnMessage")]
    pub fn set_onmessage(&self, f: &js_sys::Function) {
        let c_f = f.clone();
        if let Some(main) = self.cpu_main.as_ref() {
            main.set_onmessage(move |_| {
                let is_cpu = true;
                c_f.call1(&JsValue::null(), &is_cpu.into()).unwrap();
            });
        }
        let c_f = f.clone();
        if let Some(main) = self.gpu_main.as_ref() {
            main.set_onmessage(move |_| {
                let is_cpu = false;
                c_f.call1(&JsValue::null(), &is_cpu.into()).unwrap();
            });
        }
    }

    #[wasm_bindgen(js_name = "getResult")]
    pub fn get_result(&self, dst: &mut [u8]) -> String {
        if let Ok(mut pool) = POOL.try_lock() {
            match pool.take_ready_data() {
                ReadyData::Ready(buf) => {
                    assert!(dst.len() >= buf.len());
                    dst[..buf.len()].copy_from_slice(&buf[..]);
                    return "ready".to_owned();
                }
                ReadyData::Discarded => return "discarded".to_owned(),
                ReadyData::None => {}
            }
        }
        "none".to_owned()
    }

    #[wasm_bindgen(js_name = "calcImageOnCpu")]
    pub fn calc_image_on_cpu(
        &mut self,
        age: u32,
        x_low: f32,
        x_high: f32,
        y_low: f32,
        y_high: f32,
    ) {
        let main = self.cpu_main.as_mut().unwrap();
        main.with_app(move |mut ecs| {
            let mut data = load_to_cpu_slot(age);
            data.args = Arguments::new((x_low, x_high), (y_low, y_high));
            drop(data);
            ecs.step();
            unload_from_cpu_slot();
            web_util::worker_post_message(&JsValue::undefined()).unwrap();
        });
    }

    #[wasm_bindgen(js_name = "calcImageOnGpu")]
    pub fn calc_image_on_gpu(
        &mut self,
        age: u32,
        x_low: f32,
        x_high: f32,
        y_low: f32,
        y_high: f32,
    ) {
        let main = self.gpu_main.as_mut().unwrap();
        main.with_app(move |mut ecs| {
            let mut data = load_to_gpu_slot(age);
            data.args = Arguments::new((x_low, x_high), (y_low, y_high));
            drop(data);
            ecs.step();
        });
    }

    #[wasm_bindgen]
    pub fn destroy(&mut self) {
        self.cpu_main.take();
        self.gpu_main.take();
    }
}
