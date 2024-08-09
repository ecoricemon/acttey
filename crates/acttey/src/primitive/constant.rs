#![allow(unused)]

pub mod colors {
    use super::super::{u8x4_to_color, Color};

    /// R: 128, G: 128, B: 128
    pub const DEFAULT: Color = u8x4_to_color([0x80, 0x80, 0x80]);
    /// R: 0, G: 0, B: 0
    pub const BLACK: Color = u8x4_to_color([0x00, 0x00, 0x00]);
    /// R: 255, G: 255, B: 255
    pub const WHITE: Color = u8x4_to_color([0xFF, 0xFF, 0xFF]);
    /// R: 255, G: 0, B: 0
    pub const RED: Color = u8x4_to_color([0xFF, 0x00, 0x00]);
    /// R: 0, G: 255, B: 0
    pub const GREEN: Color = u8x4_to_color([0x00, 0xFF, 0x00]);
    /// R: 0, G: 0, B: 255
    pub const BLUE: Color = u8x4_to_color([0x00, 0x00, 0xFF]);
    /// R: 255, G: 255, B: 0
    pub const YELLOW: Color = u8x4_to_color([0xFF, 0xFF, 0x00]);
    /// R: 255, G: 0, B: 255
    pub const MAGENTA: Color = u8x4_to_color([0xFF, 0x00, 0xFF]);
    /// R: 0, G: 255, B: 255
    pub const CYAN: Color = u8x4_to_color([0x00, 0xFF, 0xFF]);
    /// R: 128, G: 128, B: 128
    pub const GRAY: Color = u8x4_to_color([0x80, 0x80, 0x80]);
    /// R: 255, G: 105, B: 180
    pub const HOTPINK: Color = u8x4_to_color([0xFF, 0x69, 0xB4]);
}

pub mod radians {
    // 0.62831853071795864769252867665590057_f32
    /// π/5 (36°)
    pub const FRAC_PI_5: f32 = 0.628_318_55_f32;
    // 1.25663706143591729538505735331180115_f32
    /// 2π/5 (72°)
    pub const FRAC_TAU_5: f32 = 1.256_637_1_f32;
    /// π/2 (90°)
    pub const FRAC_PI_2: f32 = std::f32::consts::FRAC_PI_2;
    /// π/4 (45°)
    pub const FRAC_PI_4: f32 = std::f32::consts::FRAC_PI_4;
    /// π (180°)
    pub const PI: f32 = std::f32::consts::PI;
    /// 2π (360°)
    pub const TAU: f32 = std::f32::consts::TAU;
}
