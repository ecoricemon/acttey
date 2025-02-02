//! Utility functions for web.

#![cfg(target_arch = "wasm32")]

use std::time::Duration;
use wasm_bindgen::prelude::*;

/// Calls [`web_sys::console::log_1`] for the given string.
pub fn console_log(s: String) {
    web_sys::console::log_1(&s.into());
}

/// Returns [`web_sys::DedicatedWorkerGlobalScope`].
///
/// This function must be called in worker context.
pub fn worker_global() -> web_sys::DedicatedWorkerGlobalScope {
    js_sys::global().unchecked_into::<web_sys::DedicatedWorkerGlobalScope>()
}

/// Sends the given message back.
///
/// This function must be called in worker context.
pub fn worker_post_message(msg: &JsValue) -> Result<(), JsValue> {
    worker_global().post_message(msg)
}

/// Returns a number between 1 and the number of logical processors available
/// to the user agent. It may be lower than the actual number of logical
/// processors depending on browser.
///
/// This function can be called in both window and worker contexts.
pub fn available_parallelism() -> usize {
    // `Navigator::hardware_concurrency()` gives us the number of logical
    // processors which is at least 1.
    if let Some(window) = web_sys::window() {
        let navigator = window.navigator();
        navigator.hardware_concurrency() as usize
    } else {
        let global: web_sys::WorkerGlobalScope = js_sys::global().unchecked_into();
        let navigator = global.navigator();
        navigator.hardware_concurrency() as usize
    }
}

/// Returns [`web_sys::Performance`].
///
/// This function can be called in both window and worker contexts.
pub fn performance() -> Option<web_sys::Performance> {
    if let Some(window) = web_sys::window() {
        window.performance()
    } else {
        let global: web_sys::WorkerGlobalScope = js_sys::global().unchecked_into();
        global.performance()
    }
}

/// Converts [`web_sys::Performance`] into [`Duration`].
pub fn perf_to_duration(perf: web_sys::Performance) -> Duration {
    let nanos: u64 = (perf.now() /* millis */ * 1_000_000.0) as u64;
    let secs: u64 = nanos / 1_000_000_000;
    let nanos: u64 = nanos - secs * 1_000_000_000;
    Duration::new(secs, nanos as u32)
}

/// Returns current time using web API.
pub fn now() -> Option<Duration> {
    performance().map(perf_to_duration)
}
