/// ref: https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set
use super::share::*;
use acttey::prelude::*;
use std::{
    ops::{Add, Mul},
    slice,
};

pub(super) fn calc(buf: &mut [u8], args: Arguments) {
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
            *pixel = PALETTE[iter as usize];
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
