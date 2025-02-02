use acttey::prelude::*;
use std::{
    array, mem,
    ops::{Deref, DerefMut},
    slice,
    sync::{LazyLock, Mutex},
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen(js_name = "canvasWidth")]
pub fn canvas_width() -> u32 {
    1024
}

#[wasm_bindgen(js_name = "canvasHeight")]
pub fn canvas_height() -> u32 {
    canvas_width()
}

pub(super) const MAX_ITER: u32 = 100;

#[wasm_bindgen(js_name = "startAge")]
pub fn start_age() -> u32 {
    0
}

#[wasm_bindgen(js_name = "numCpus")]
pub fn num_cpus() -> u32 {
    web_util::available_parallelism() as u32
}

pub(super) fn cpu_slot() -> Guard<'static, SlotData> {
    Guard(CPU_SLOT.lock().unwrap())
}

pub(super) fn gpu_slot() -> Guard<'static, SlotData> {
    Guard(GPU_SLOT.lock().unwrap())
}

pub(super) fn load_to_cpu_slot(age: u32) -> Guard<'static, SlotData> {
    let mut pool = POOL.lock().unwrap();
    pool.load(true, age)
}

pub(super) fn load_to_gpu_slot(age: u32) -> Guard<'static, SlotData> {
    let mut pool = POOL.lock().unwrap();
    pool.load(false, age)
}

pub(super) fn unload_from_cpu_slot() {
    let mut pool = POOL.lock().unwrap();
    pool.unload(true);
}

pub(super) fn unload_from_gpu_slot() {
    let mut pool = POOL.lock().unwrap();
    pool.unload(false);
}

pub(super) fn clear_global() {
    *CPU_SLOT.lock().unwrap() = None;
    *GPU_SLOT.lock().unwrap() = None;
    POOL.lock().unwrap().clear();
}

pub(super) struct Guard<'a, T: 'a>(std::sync::MutexGuard<'a, Option<T>>);

impl<'a, T: 'a> Deref for Guard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}

impl<'a, T: 'a> DerefMut for Guard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}

static CPU_SLOT: LazyLock<Mutex<Option<SlotData>>> = LazyLock::new(|| Mutex::new(None));
static GPU_SLOT: LazyLock<Mutex<Option<SlotData>>> = LazyLock::new(|| Mutex::new(None));
pub(super) static POOL: LazyLock<Mutex<Pool>> = LazyLock::new(|| Mutex::new(Pool::new()));

pub(super) struct Pool {
    idle: Vec<SlotData>,
    ready: Vec<SlotData>,
    temp: Vec<SlotData>,
    next_age: u32,
}

impl Pool {
    fn new() -> Self {
        Self {
            idle: Vec::new(),
            ready: Vec::new(),
            temp: Vec::new(),
            next_age: start_age(),
        }
    }

    pub(super) fn take_ready_data(&mut self) -> ReadyData {
        if let Some(data) = self.ready.first() {
            if data.age >= self.next_age {
                self.next_age = data.age + 1;
                self.temp.push(self.ready.remove(0));
                let data = self.temp.last().unwrap();
                ReadyData::Ready(data.buf.as_slice())
            } else {
                // CPU + GPU: Stale data, especially by CPU, will be discarded.
                self.temp.push(self.ready.remove(0));
                ReadyData::Discarded
            }
        } else {
            ReadyData::None
        }
    }

    fn clear(&mut self) {
        drop(mem::take(self));
    }

    fn load(&mut self, is_cpu: bool, age: u32) -> Guard<'static, SlotData> {
        while let Some(data) = self.temp.pop() {
            self.idle.push(data);
        }

        if self.idle.is_empty() {
            self.idle.push(SlotData::new());
        }
        let mut data = self.idle.pop().unwrap();
        data.age = age;

        let dst = if is_cpu { &CPU_SLOT } else { &GPU_SLOT };
        let mut slot = dst.lock().unwrap();
        assert!(slot.is_none());
        *slot = Some(data);
        Guard(slot)
    }

    fn unload(&mut self, is_cpu: bool) {
        let src = if is_cpu { &CPU_SLOT } else { &GPU_SLOT };
        let mut slot = src.lock().unwrap();
        let data = slot.take().unwrap();
        self.ready.push(data);
    }
}

impl Default for Pool {
    fn default() -> Self {
        Self::new()
    }
}

pub(super) struct SlotData {
    pub(super) args: Arguments,
    pub(super) buf: Vec<u8>,
    pub(super) age: u32,
}

impl SlotData {
    fn new() -> Self {
        Self {
            args: Arguments::new((0.0, 0.0), (0.0, 0.0)),
            buf: vec![0; (canvas_width() * canvas_height() * 4) as usize],
            age: 0,
        }
    }
}

pub(super) enum ReadyData<'a> {
    Ready(&'a [u8]),
    Discarded,
    None,
}

// Must be able to be divided by 16 for GPU uniform buffer.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub(super) struct Arguments {
    pub(super) size: (u32, u32),
    pub(super) x_range: (f32, f32),
    pub(super) y_range: (f32, f32),
    _pad: [u32; 2],
}

impl Arguments {
    pub(super) fn new(x_range: (f32, f32), y_range: (f32, f32)) -> Self {
        Self {
            size: (canvas_width(), canvas_height()),
            x_range,
            y_range,
            _pad: [0; 2],
        }
    }

    pub(super) fn as_u8_slice(&self) -> &[u8] {
        let ptr = self as *const Self as *const u8;
        let len = mem::size_of::<Arguments>();
        unsafe { slice::from_raw_parts(ptr, len) }
    }
}

pub(super) static PALETTE: LazyLock<[u32; MAX_ITER as usize]> = LazyLock::new(|| {
    let mut palette = array::from_fn(|i| {
        let (r, g, b) = iter_to_rgb(i as u32);
        if is_little_endian() {
            255 << 24 | ((b as u32) << 16) | ((g as u32) << 8) | ((r as u32) << 0)
        } else {
            (r as u32) << 24 | (g as u32) << 16 | (b as u32) << 8 | 255
        }
    });

    // black
    palette[MAX_ITER as usize - 1] = if is_little_endian() { 255 << 24 } else { 255 };

    palette
});

pub(super) fn scale(val: u32, val_limit: u32, low: f32, high: f32) -> f32 {
    (val as f32 / val_limit as f32) * (high - low) + low
}

fn iter_to_rgb(i: u32) -> (u8, u8, u8) {
    let h = scale(i, MAX_ITER, 0.0, 360.0);
    let s = 0.6;
    let v = scale(i, MAX_ITER, 0.0, 1.0);
    hsv_to_rgb(h, s, v)
}

// h: [0, 360), s: [0, 1], v: [0, 1]
fn hsv_to_rgb(h: f32, s: f32, v: f32) -> (u8, u8, u8) {
    let c = v * s;
    let x = c * (1.0 - ((h / 60.0) % 2.0 - 1.0).abs());
    let m = v - c;

    let (r, g, b) = match h as u32 {
        0..=59 => (c, x, 0.0),
        60..=119 => (x, c, 0.0),
        120..=179 => (0.0, c, x),
        180..=239 => (0.0, x, c),
        240..=299 => (x, 0.0, c),
        _ => (c, 0.0, x),
    };

    (
        ((r + m) * 255.0) as u8,
        ((g + m) * 255.0) as u8,
        ((b + m) * 255.0) as u8,
    )
}

fn is_little_endian() -> bool {
    u16::from_ne_bytes([1, 0]) == u16::from_le_bytes([1, 0])
}
