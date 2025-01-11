#![cfg(target_arch = "wasm32")]

use std::time::Duration;
use wasm_bindgen::prelude::*;

pub fn worker_global() -> web_sys::DedicatedWorkerGlobalScope {
    js_sys::global().unchecked_into::<web_sys::DedicatedWorkerGlobalScope>()
}

pub fn worker_post_message(msg: &JsValue) -> Result<(), JsValue> {
    worker_global().post_message(msg)
}

/// Returns a number between 1 and the number of logical processors available
/// to the user agent. It may be lower than the actual number of logical
/// processors depending on browser.
pub fn available_parallelism() -> usize {
    if let Some(window) = web_sys::window() {
        let navigator = window.navigator();
        navigator.hardware_concurrency() as usize
    } else {
        let global: web_sys::WorkerGlobalScope = js_sys::global().unchecked_into();
        let navigator = global.navigator();
        navigator.hardware_concurrency() as usize
    }
}

pub fn performance() -> Option<web_sys::Performance> {
    if let Some(window) = web_sys::window() {
        window.performance()
    } else {
        let global: web_sys::WorkerGlobalScope = js_sys::global().unchecked_into();
        global.performance()
    }
}

pub fn perf_to_duration(perf: web_sys::Performance) -> Duration {
    let nanos: u64 = (perf.now() /* millis */ * 1_000_000.0) as u64;
    let secs: u64 = nanos / 1_000_000_000;
    let nanos: u64 = nanos - secs * 1_000_000_000;
    Duration::new(secs, nanos as u32)
}

pub fn now() -> Option<Duration> {
    performance().map(perf_to_duration)
}
