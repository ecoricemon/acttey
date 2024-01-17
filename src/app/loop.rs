use wasm_bindgen::prelude::*;

pub struct Loop {
    window: web_sys::Window,
    callback: Closure<dyn FnMut(f64)>,
}

impl Loop {
    pub fn new() -> Self {
        Self {
            window: crate::util::get_window(),
            callback: Closure::new(|_| {}),
        }
    }

    pub fn set_callback(&mut self, callback: Closure<dyn FnMut(f64)>) {
        self.callback = callback;
    }

    #[inline]
    pub fn request_animation_frame(&self) {
        self.window
            .request_animation_frame(self.callback.as_ref().unchecked_ref())
            .expect_throw(crate::errmsg::WEBSYS_REQ_ANIMATION);
    }
}
