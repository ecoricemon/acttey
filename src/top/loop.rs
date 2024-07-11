use wasm_bindgen::{closure::Closure, JsCast, UnwrapThrowExt};

pub struct Loop {
    global: web_sys::DedicatedWorkerGlobalScope,
    callback: Closure<dyn FnMut(f64)>,
}

impl Loop {
    pub fn new() -> Self {
        Self {
            global: js_sys::global().unchecked_into(),
            callback: Closure::new(|_| {}),
        }
    }

    pub fn set_callback(&mut self, callback: Closure<dyn FnMut(f64)>) {
        self.callback = callback;
    }

    pub fn request_animation_frame(&self) {
        self.global
            .request_animation_frame(self.callback.as_ref().unchecked_ref())
            .expect_throw(crate::errmsg::WEBSYS_REQ_ANIMATION);
    }
}
