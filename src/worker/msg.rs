use paste::paste;
use std::mem;
use wasm_bindgen::{JsCast, JsValue};

#[repr(C)]
union Primitive {
    i8: i8,
    u8: u8,
    i16: i16,
    u16: u16,
    i32: i32,
    u32: u32,
    f32: f32,
    i64: i64,
    u64: u64,
    f64: f64,
}
/// Declares `transmute_<>_to_f64` and `transmute_f64_to_<>`.
/// Those functions copy data in bit level.  
/// Only pimitive types, that are smaller than or equal to f64, are allowed.
macro_rules! decl_transmute_f64 {
    ($ty:ty) => {
        paste! {
            #[allow(dead_code)]
            #[inline]
            fn [<transmute_$ty _to_f64>](value: $ty) -> f64 {
                unsafe { Primitive { $ty: value }.f64 }
            }

            #[allow(dead_code)]
            #[inline]
            fn [<transmute_f64_to_$ty>](value: f64) -> $ty {
                unsafe { Primitive { f64: value }.$ty }
            }
        }
    };
}

decl_transmute_f64!(i8);
decl_transmute_f64!(i16);
decl_transmute_f64!(u32);
decl_transmute_f64!(u64);

// TODO: Pack with other fields?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JsMsgHeader(pub u64);

impl JsMsgHeader {
    /// Common message group.
    const COMMON: u64 = 0;
    /// Window message group.
    const WINDOW: u64 = 1 << 8;
    /// Mouse message group.
    const MOUSE: u64 = 2 << 8;

    /// --- Common message group starts from here --- ///

    pub const INIT_INNER: u64 = Self::COMMON | 1;
    pub const INIT: Self = Self(Self::INIT_INNER);

    pub const CANVAS_INNER: u64 = Self::INIT_INNER + 1;
    pub const CANVAS: Self = Self(Self::CANVAS_INNER);

    pub const FN_INNER: u64 = Self::CANVAS_INNER + 1;
    pub const FN: Self = Self(Self::FN_INNER);

    /// --- Window message group starts from here --- ///

    pub const WINDOW_SCALE_INNER: u64 = Self::WINDOW | 1;
    pub const WINDOW_SCALE: Self = Self(Self::WINDOW_SCALE_INNER);

    pub const CANVAS_RESIZE_INNER: u64 = Self::WINDOW_SCALE_INNER + 1;
    pub const CANVAS_RESIZE: Self = Self(Self::CANVAS_RESIZE_INNER);

    /// --- Mouse message group starts from here --- ///

    pub const MOUSE_MOVE_INNER: u64 = Self::MOUSE | 1;
    pub const MOUSE_MOVE: Self = Self(Self::MOUSE_MOVE_INNER);

    pub const MOUSE_CLICK_INNER: u64 = Self::MOUSE_MOVE_INNER + 1;
    pub const MOUSE_CLICK: Self = Self(Self::MOUSE_CLICK_INNER);

    #[inline]
    pub fn into_js_value(self) -> JsValue {
        JsValue::from_f64(transmute_u64_to_f64(self.0))
    }

    #[inline]
    pub fn from_js_value(value: JsValue) -> Self {
        let value = value.as_f64().unwrap();
        Self(transmute_f64_to_u64(value))
    }
}

const UNIT_SIZE: usize = mem::size_of::<f64>();
const ROUND_UP: usize = UNIT_SIZE - 1;
const HEADER_LEN: usize = (mem::size_of::<JsMsgHeader>() + ROUND_UP) / UNIT_SIZE;

/// Checks if f64 messge's `required_array_len()` is sufficient or not to avoid mistakes in compile time.
macro_rules! assert_required_array_len {
    ($ty:ty) => {
        const _: () = {
            // Compares including padding bytess.
            const BODY_LEN: usize = (mem::size_of::<$ty>() + ROUND_UP) / UNIT_SIZE;

            assert!(<$ty>::required_array_len() as usize == HEADER_LEN + BODY_LEN)
        };
    };
}

/// Instant message for initializing main worker.
#[derive(Debug, Clone)]
pub struct JsMsgInit(pub JsMsgCanvas /* dummy canvas */);

impl JsMsgInit {
    const HEADER: JsMsgHeader = JsMsgHeader::INIT;

    #[inline]
    const fn required_array_len_wo_header() -> u32 {
        // 2
        JsMsgCanvas::required_array_len_wo_header()
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn required_array_len() -> u32 {
        // 3
        Self::required_array_len_wo_header() + HEADER_LEN as u32
    }

    pub fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::required_array_len());
        Self::write_header(&buf);
        let tr = self.0.write_body(&buf);
        worker.post_message_with_transfer(&buf, &tr).unwrap();
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, JsMsgHeader::from_js_value(buf.get(0)));
        Self(JsMsgCanvas::_read_body(buf))
    }
}

/// Instant message for sending offscreen canvas from window to main worker.
#[derive(Debug, Clone)]
pub struct JsMsgCanvas {
    pub element: web_sys::OffscreenCanvas,
    pub handle: u32,
}

impl JsMsgCanvas {
    const HEADER: JsMsgHeader = JsMsgHeader::CANVAS;

    #[inline]
    const fn required_array_len_wo_header() -> u32 {
        2
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn required_array_len() -> u32 {
        Self::required_array_len_wo_header() + HEADER_LEN as u32
    }

    pub fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::required_array_len());
        Self::write_header(&buf);
        let tr = self.write_body(&buf);
        worker.post_message_with_transfer(&buf, &tr).unwrap();
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[must_use]
    fn write_body(self, buf: &js_sys::Array) -> js_sys::Array {
        buf.set(1, JsValue::from(self.element.clone()));
        buf.set(2, JsValue::from_f64(transmute_u32_to_f64(self.handle)));
        let tr = js_sys::Array::new_with_length(1);
        tr.set(0, JsValue::from(self.element));
        tr
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, JsMsgHeader::from_js_value(buf.get(0)));
        Self::_read_body(buf)
    }

    #[inline]
    fn _read_body(buf: &js_sys::Array) -> Self {
        Self {
            element: buf.get(1).unchecked_into(),
            handle: transmute_f64_to_u32(buf.get(2).as_f64().unwrap()),
        }
    }
}

/// Instant message for sending a function.
#[derive(Debug, Clone, Copy)]
pub struct JsMsgFn {
    /// Platform agnostic function pointer converted from fn(), fn(A), fn(A, B) and so on.
    pub f: u64,

    /// Discriminant of function pointer.
    /// Use [`JsMsgFnDisc`] for this value.
    pub disc: u32,
}

#[repr(C)]
union JsMsgFnCodec {
    src: JsMsgFn,
    dst: [f64; JsMsgFn::required_array_len_wo_header() as usize],
}

impl JsMsgFn {
    const HEADER: JsMsgHeader = JsMsgHeader::FN;

    #[inline]
    const fn required_array_len_wo_header() -> u32 {
        // 2
        ((mem::size_of::<Self>() + ROUND_UP) / UNIT_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    pub const fn required_array_len() -> u32 {
        // 3
        Self::required_array_len_wo_header() + HEADER_LEN as u32
    }

    pub fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::required_array_len());
        Self::write_header(&buf);
        self.write_body(&buf);
        worker.post_message(&buf).unwrap();
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    fn write_body(&self, buf: &js_sys::Array) {
        let encoded = self.encode();
        for (i, elem) in encoded.into_iter().enumerate() {
            buf.set((i + HEADER_LEN) as u32, JsValue::from_f64(elem));
        }
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, JsMsgHeader::from_js_value(buf.get(0)));
        Self::_read_body(buf)
    }

    #[inline]
    fn _read_body(buf: &js_sys::Array) -> Self {
        Self::decode([buf.get(1).as_f64().unwrap(), buf.get(2).as_f64().unwrap()])
    }

    #[inline]
    const fn encode(self) -> [f64; Self::required_array_len_wo_header() as usize] {
        unsafe { JsMsgFnCodec { src: self }.dst }
    }

    #[inline]
    const fn decode(encoded: [f64; Self::required_array_len_wo_header() as usize]) -> Self {
        unsafe { JsMsgFnCodec { dst: encoded }.src }
    }
}

assert_required_array_len!(JsMsgFn);

/// Used for [`JsMsgFn::disc`].
pub enum JsMsgFnDisc {
    /// acttey uses from `Begin`.
    Begin = 0x1000_0000,
    UserInit,
}

/// Message for sending current window scale factor (device pixel ratio).
#[derive(Debug, Clone, Copy)]
pub struct JsMsgWindowScale {
    pub scale: f64,
}

impl JsMsgWindowScale {
    const HEADER: JsMsgHeader = JsMsgHeader::WINDOW_SCALE;

    pub fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::required_array_len());
        Self::write_header(&buf);
        buf
    }

    #[inline]
    const fn required_array_len_wo_header() -> u32 {
        // 1
        ((mem::size_of::<Self>() + ROUND_UP) / UNIT_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    pub const fn required_array_len() -> u32 {
        // 2
        Self::required_array_len_wo_header() + HEADER_LEN as u32
    }

    #[inline]
    pub fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    pub fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        self._write_body(buf)
    }

    #[inline]
    fn _write_body(&self, buf: &js_sys::Array) {
        buf.set(1, JsValue::from_f64(self.scale));
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, JsMsgHeader::from_js_value(buf.get(0)));
        Self::_read_body(buf)
    }

    #[inline]
    fn _read_body(buf: &js_sys::Array) -> Self {
        Self {
            scale: buf.get(1).as_f64().unwrap(),
        }
    }
}

assert_required_array_len!(JsMsgWindowScale);

/// Message for sending current canvas size according to window resize event.
#[derive(Debug, Clone, Copy)]
pub struct JsMsgCanvasResize {
    pub handle: u32,
    pub width: i16,
    pub height: i16,
}

#[repr(C)]
union JsMsgCanvasResizeCodec {
    src: JsMsgCanvasResize,
    dst: [f64; JsMsgCanvasResize::required_array_len_wo_header() as usize],
}

impl JsMsgCanvasResize {
    const HEADER: JsMsgHeader = JsMsgHeader::CANVAS_RESIZE;

    pub fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::required_array_len());
        Self::write_header(&buf);
        buf
    }

    #[inline]
    const fn required_array_len_wo_header() -> u32 {
        // 1
        ((mem::size_of::<Self>() + ROUND_UP) / UNIT_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    pub const fn required_array_len() -> u32 {
        // 2
        Self::required_array_len_wo_header() + HEADER_LEN as u32
    }

    #[inline]
    pub fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    pub fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        self._write_body(buf)
    }

    #[inline]
    fn _write_body(&self, buf: &js_sys::Array) {
        let encoded = self.encode();
        for (i, elem) in encoded.into_iter().enumerate() {
            buf.set((i + HEADER_LEN) as u32, JsValue::from_f64(elem));
        }
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, JsMsgHeader::from_js_value(buf.get(0)));
        Self::_read_body(buf)
    }

    #[inline]
    fn _read_body(buf: &js_sys::Array) -> Self {
        Self::decode([buf.get(1).as_f64().unwrap()])
    }

    #[inline]
    const fn encode(self) -> [f64; Self::required_array_len_wo_header() as usize] {
        unsafe { JsMsgCanvasResizeCodec { src: self }.dst }
    }

    #[inline]
    const fn decode(encoded: [f64; Self::required_array_len_wo_header() as usize]) -> Self {
        unsafe { JsMsgCanvasResizeCodec { dst: encoded }.src }
    }
}

assert_required_array_len!(JsMsgCanvasResize);

/// Message for other mouse messages.
/// Field types may shrink compared to types in [`web_sys::MouseEvent`].
#[derive(Debug, Clone, Copy)]
struct JsMsgMouse {
    pub button: i8,
    pub client_x: i16,
    pub client_y: i16,
    pub movement_x: i16,
    pub movement_y: i16,
    pub offset_x: i16,
    pub offset_y: i16,
}

#[repr(C)]
union JsMsgMouseCodec {
    src: JsMsgMouse,
    dst: [f64; JsMsgMouse::required_array_len_wo_header() as usize],
}

impl JsMsgMouse {
    #[inline]
    const fn required_array_len_wo_header() -> u32 {
        // 2
        ((mem::size_of::<Self>() + ROUND_UP) / UNIT_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn required_array_len() -> u32 {
        // 3
        Self::required_array_len_wo_header() + HEADER_LEN as u32
    }

    #[inline]
    fn write_body(self, buf: &js_sys::Array) {
        let encoded = self.encode();
        for (i, elem) in encoded.into_iter().enumerate() {
            buf.set((i + HEADER_LEN) as u32, JsValue::from_f64(elem));
        }
    }

    #[inline]
    fn read_body(buf: &js_sys::Array) -> Self {
        Self::decode([buf.get(1).as_f64().unwrap(), buf.get(2).as_f64().unwrap()])
    }

    #[inline]
    const fn encode(self) -> [f64; Self::required_array_len_wo_header() as usize] {
        unsafe { JsMsgMouseCodec { src: self }.dst }
    }

    #[inline]
    const fn decode(encoded: [f64; Self::required_array_len_wo_header() as usize]) -> Self {
        unsafe { JsMsgMouseCodec { dst: encoded }.src }
    }
}

assert_required_array_len!(JsMsgMouse);

/// Message for seding mouse move event to main worker.
/// This structure is a wrapper of common mouse message [`JsMsgMouse`].
#[derive(Debug, Clone, Copy)]
pub struct JsMsgMouseMove(JsMsgMouse);

impl JsMsgMouseMove {
    const HEADER: JsMsgHeader = JsMsgHeader::MOUSE_MOVE;

    pub fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::required_array_len());
        Self::write_header(&buf);
        buf
    }

    #[inline]
    pub const fn required_array_len() -> u32 {
        JsMsgMouse::required_array_len()
    }

    #[inline]
    pub fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    pub fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        self._write_body(buf)
    }

    #[inline]
    fn _write_body(&self, buf: &js_sys::Array) {
        self.0.write_body(buf);
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, JsMsgHeader::from_js_value(buf.get(0)));
        Self::_read_body(buf)
    }

    #[inline]
    fn _read_body(buf: &js_sys::Array) -> Self {
        Self(JsMsgMouse::read_body(buf))
    }
}

/// Message for sending mouse click event to main worker.
/// This structure is a wrapper of common mouse message [`JsMsgMouse`].
#[derive(Debug, Clone, Copy)]
pub struct JsMsgMouseClick(JsMsgMouse);

impl JsMsgMouseClick {
    const HEADER: JsMsgHeader = JsMsgHeader::MOUSE_CLICK;

    #[inline]
    pub const fn required_array_len() -> u32 {
        JsMsgMouse::required_array_len()
    }

    #[inline]
    pub fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    pub fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        self._write_body(buf);
    }

    #[inline]
    fn _write_body(&self, buf: &js_sys::Array) {
        self.0.write_body(buf);
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, JsMsgHeader::from_js_value(buf.get(0)));
        Self::_read_body(buf)
    }

    #[inline]
    fn _read_body(buf: &js_sys::Array) -> Self {
        Self(JsMsgMouse::read_body(buf))
    }
}
