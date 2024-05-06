use crate::ds::borrow::Atomic;
use std::sync::atomic::{AtomicI32, Ordering};

// TODO: Delete and use `AtomicI32` statically once it's been stable on both single & multi threads.
#[derive(Debug)]
pub struct JsAtomic(AtomicI32);

impl Atomic<i32> for JsAtomic {
    #[inline]
    fn new(value: i32) -> Self {
        Self(AtomicI32::new(value))
    }

    #[inline]
    fn load(&self, order: Ordering) -> i32 {
        self.0.load(order)
    }

    #[inline]
    fn add(&self, value: i32, order: Ordering) -> i32 {
        self.0.add(value, order)
    }

    #[inline]
    fn sub(&self, value: i32, order: Ordering) -> i32 {
        self.0.sub(value, order)
    }

    #[inline]
    fn compare_exchange(
        &self,
        expect: i32,
        replace: i32,
        success: Ordering,
        failure: Ordering,
    ) -> Result<i32, i32> {
        self.0.compare_exchange(expect, replace, success, failure)
    }
}

// use crate::{ds::borrow::Atomic, util::web};
// use js_sys::{Atomics, Int32Array, SharedArrayBuffer};
// use std::{mem, sync::atomic::Ordering};

// #[derive(Debug)]
// pub struct JsAtomic {
//     array: js_sys::Int32Array,
// }

// impl Atomic<i32> for JsAtomic {
//     fn new(value: i32) -> Self {
//         // Note.
//         // I think this checking is unnecessary because if wasm is built for multithread, then wasm-bindgen makes the whole Rust side memory SharedArrayBuffer.
//         // But, checking is not a burden.
//         // Plus, we can use Atomic functions on plain Int32Array.
//         const LEN: u32 = 1; // Single element array is inefficient, but it's easy to use.
//         let array = if web::cross_origin_isolated() {

//             // TODO: [BUG] It seems this is not shared across workers.
//             // Do I have to use postMessage to share this?
//             let shared = SharedArrayBuffer::new(LEN * mem::size_of::<i32>() as u32);
//             Int32Array::new(&shared)

//         } else {
//             Int32Array::new(&LEN.into())
//         };

//         // Initializes the array.
//         array.fill(value, 0, LEN);

//         Self { array }
//     }

//     #[inline]
//     fn load(&self, _: Ordering) -> i32 {
//         Atomics::load(&self.array, 0).unwrap()
//     }

//     #[inline]
//     fn add(&self, value: i32, _: Ordering) -> i32 {
//         Atomics::add(&self.array, 0, value).unwrap()
//     }

//     #[inline]
//     fn sub(&self, value: i32, _: Ordering) -> i32 {
//         Atomics::sub(&self.array, 0, value).unwrap()
//     }

//     #[inline]
//     fn compare_exchange(
//         &self,
//         expect: i32,
//         replace: i32,
//         _: Ordering,
//         _: Ordering,
//     ) -> Result<i32, i32> {
//         let old = Atomics::compare_exchange(&self.array, 0, expect, replace).unwrap();
//         if old == expect {
//             Ok(old)
//         } else {
//             Err(old)
//         }
//     }
// }
