/// ref: https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set
use acttey::prelude::*;
use std::{
    array, mem,
    ops::{Add, Mul},
    slice,
    sync::{LazyLock, Mutex},
};

// The bigger the better for GPU.
pub(super) const MAX_ITER: u32 = 256;

pub(super) static ARGS: LazyLock<Mutex<Arguments>> = LazyLock::new(|| Mutex::new(Arguments::new()));
pub(super) static BUF: LazyLock<Mutex<Vec<u8>>> = LazyLock::new(|| Mutex::new(vec![]));

// Must be able to be divided by 16 for GPU uniform buffer.
#[repr(C)]
pub(super) struct Arguments {
    pub(super) palette: [u32; MAX_ITER as usize],
    pub(super) size: (u32, u32),
    pub(super) x_range: (f32, f32),
    pub(super) y_range: (f32, f32),
    _pad: [u32; 2],
}

impl Arguments {
    pub(super) fn new() -> Self {
        let mut palette = array::from_fn(|i| {
            let (r, g, b) = Self::iter_to_rgb(i as u32);
            if Self::is_little_endian() {
                255 << 24 | ((b as u32) << 16) | ((g as u32) << 8) | ((r as u32) << 0)
            } else {
                (r as u32) << 24 | (g as u32) << 16 | (b as u32) << 8 | 255
            }
        });
        // black
        palette[MAX_ITER as usize - 1] = if Self::is_little_endian() {
            255 << 24
        } else {
            255
        };

        Self {
            palette,
            size: (0, 0),
            x_range: (0.0, 0.0),
            y_range: (0.0, 0.0),
            _pad: [0; 2],
        }
    }

    pub(super) fn as_u8_slice(&self) -> &[u8] {
        let ptr = self as *const Self as *const u8;
        let len = mem::size_of::<Arguments>();
        unsafe { slice::from_raw_parts(ptr, len) }
    }

    // Returns offset in bytes and data of 'size, x_range, y_range' in order.
    pub(super) fn size_xrange_yrange(&self) -> (usize, &[u8]) {
        let offset = mem::offset_of!(Arguments, size);
        let ptr = self as *const Self as *const u8;
        let ptr_off = unsafe { ptr.add(offset) };
        let len = mem::size_of::<Arguments>() - offset;
        let data = unsafe { slice::from_raw_parts(ptr_off, len) };
        (offset, data)
    }

    fn iter_to_rgb(i: u32) -> (u8, u8, u8) {
        let h = scale(i, MAX_ITER, 0.0, 360.0);
        let s = 0.6;
        let v = scale(i, MAX_ITER, 0.0, 1.0);
        Self::hsv_to_rgb(h, s, v)
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
}

pub(super) fn calc(buf: &mut [u8], args: &Arguments) {
    assert!(args.size.0 * args.size.1 * 4 <= buf.len() as u32);
    let buf = unsafe { slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut u32, buf.len() / 4) };
    (0..args.size.0 * args.size.1)
        .into_par_iter()
        .zip(buf)
        .into_ecs_par()
        .for_each(|(i, pixel)| {
            let x = i % args.size.0;
            let y = i / args.size.0;
            let iter = calc_pixel(x, y, args.size, args.x_range, args.y_range);
            *pixel = args.palette[iter as usize];
        });
}

fn calc_pixel(x: u32, y: u32, size: (u32, u32), x_range: (f32, f32), y_range: (f32, f32)) -> u32 {
    let c = Complex {
        r: scale(x, size.0, x_range.0, x_range.1),
        i: scale(y, size.1, y_range.0, y_range.1),
    };
    let mut z = Complex { r: 0.0, i: 0.0 };
    let mut iter = 0;
    while iter < (MAX_ITER - 1) && z.norm_sqr() < 4.0 {
        z = z * z + c;
        iter += 1;
    }
    iter
}

fn scale(val: u32, val_limit: u32, low: f32, high: f32) -> f32 {
    (val as f32 / val_limit as f32) * (high - low) + low
}

#[derive(Clone, Copy)]
struct Complex {
    r: f32,
    i: f32,
}

impl Complex {
    fn norm_sqr(self) -> f32 {
        self.r * self.r + self.i * self.i
    }
}

impl Add for Complex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            r: self.r + rhs.r,
            i: self.i + rhs.i,
        }
    }
}

impl Mul for Complex {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            r: self.r * rhs.r - self.i * rhs.i,
            i: self.r * rhs.i + self.i * rhs.r,
        }
    }
}
