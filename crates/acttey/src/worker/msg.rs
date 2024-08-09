use crate::render::canvas::CanvasHandle;
use std::{fmt::Debug, mem};
use wasm_bindgen::{JsCast, JsValue};

/// Message event header.  
/// Inner value will be transmuted into f64 and vice versa.
/// You must guarantee that f64 representation is not Nan (or Inf).
/// If so, someone(maybe JS) will change it's bits except Nan bits.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct MsgHeader(pub u64);

impl MsgHeader {
    /// Common message group.
    const COMMON: u64 = 0;
    /// Window message group.
    const WINDOW: u64 = 1 << 8;
    /// Mouse message group.
    const MOUSE: u64 = 2 << 8;

    /// --- Common message group starts from here --- ///

    pub(crate) const INIT_INNER: u64 = Self::COMMON | 1;
    pub(crate) const INIT: Self = Self(Self::INIT_INNER);

    pub(crate) const EVENTS_INNER: u64 = Self::INIT_INNER + 1;
    pub(crate) const EVENTS: Self = Self(Self::EVENTS_INNER);

    pub(crate) const CANVAS_INNER: u64 = Self::EVENTS_INNER + 1;
    pub(crate) const CANVAS: Self = Self(Self::CANVAS_INNER);

    pub(crate) const FN_INNER: u64 = Self::CANVAS_INNER + 1;
    pub(crate) const FN: Self = Self(Self::FN_INNER);

    pub(crate) const RUN_INNER: u64 = Self::FN_INNER + 1;
    pub(crate) const RUN: Self = Self(Self::RUN_INNER);

    /// --- Window message group starts from here --- ///

    pub(crate) const WINDOW_SCALE_INNER: u64 = Self::WINDOW | 1;
    pub(crate) const WINDOW_SCALE: Self = Self(Self::WINDOW_SCALE_INNER);

    pub(crate) const CANVAS_RESIZE_INNER: u64 = Self::WINDOW_SCALE_INNER + 1;
    pub(crate) const CANVAS_RESIZE: Self = Self(Self::CANVAS_RESIZE_INNER);

    /// --- Mouse message group starts from here --- ///

    pub(crate) const MOUSE_MOVE_INNER: u64 = Self::MOUSE | 1;
    pub(crate) const MOUSE_MOVE: Self = Self(Self::MOUSE_MOVE_INNER);

    pub(crate) const CLICK_INNER: u64 = Self::MOUSE_MOVE_INNER + 1;
    pub(crate) const CLICK: Self = Self(Self::CLICK_INNER);

    pub(crate) fn into_js_value(self) -> JsValue {
        let value = f64::from_bits(self.0);
        debug_assert!(value.is_finite()); // Nan or Inf are now allowed.
        JsValue::from_f64(value)
    }

    pub(crate) fn from_js_value(value: JsValue) -> Self {
        Self(value.unchecked_into_f64().to_bits())
    }
}

const F64_SIZE: usize = mem::size_of::<f64>();
const F64_ROUND_UP: usize = F64_SIZE - 1;
const HEADER_LEN: usize = (mem::size_of::<MsgHeader>() + F64_ROUND_UP) / F64_SIZE;
const U32_SIZE: usize = mem::size_of::<u32>();
const U32_ROUND_UP: usize = U32_SIZE - 1;

/// Instant message for initializing main worker.
#[derive(Debug, Clone)]
pub(crate) struct MsgInit(pub(crate) MsgCanvas /* dummy canvas */);

impl MsgInit {
    const HEADER: MsgHeader = MsgHeader::INIT;

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        MsgCanvas::message_array_len()
    }

    pub(crate) fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        let tr = self.0.write_body(&buf);
        worker.post_message_with_transfer(&buf, &tr).unwrap();
    }

    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    pub(crate) fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));
        Self(MsgCanvas::_read_body(buf))
    }
}

/// Instant message fot sending listening events.
#[derive(Debug, Clone)]
pub(crate) struct MsgEvents {
    pub(crate) events: Vec<String>,
}

impl MsgEvents {
    const HEADER: MsgHeader = MsgHeader::EVENTS;

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        // 2
        const NUM_OF_FIELDS: u32 = 1;
        HEADER_LEN as u32 + NUM_OF_FIELDS
    }

    pub(crate) fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        self.write_body(&buf);
        worker.post_message(&buf).unwrap();
    }

    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    fn write_body(self, buf: &js_sys::Array) {
        buf.set(1, vec_string_to_js(self.events));
    }

    pub(crate) fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));
        Self {
            events: js_to_vec_string(buf.get(1)),
        }
    }
}

/// Instant message for sending offscreen canvas from window to main worker.
#[derive(Debug, Clone)]
pub(crate) struct MsgCanvas {
    pub(crate) element: web_sys::OffscreenCanvas,
    pub(crate) handle: CanvasHandle,
    pub(crate) selectors: String,
    pub(crate) scale: f64,
}

impl MsgCanvas {
    const HEADER: MsgHeader = MsgHeader::CANVAS;

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        // 5
        const NUM_OF_FIELDS: u32 = 4;
        HEADER_LEN as u32 + NUM_OF_FIELDS
    }

    pub(crate) fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        let tr = self.write_body(&buf);
        worker.post_message_with_transfer(&buf, &tr).unwrap();
    }

    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    fn write_body(self, buf: &js_sys::Array) -> js_sys::Array {
        buf.set(1, JsValue::from(self.element.clone()));
        buf.set(
            2,
            JsValue::from_f64(f64::from_bits(self.handle.unwrap() as u64)),
        );
        buf.set(3, JsValue::from_str(&self.selectors));
        buf.set(4, JsValue::from_f64(self.scale));
        let tr = js_sys::Array::new_with_length(1);
        tr.set(0, JsValue::from(self.element));
        tr
    }

    pub(crate) fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));
        Self::_read_body(buf)
    }

    fn _read_body(buf: &js_sys::Array) -> Self {
        Self {
            element: buf.get(1).unchecked_into(),
            handle: CanvasHandle::new(buf.get(2).unchecked_into_f64().to_bits() as u32),
            selectors: buf.get(3).as_string().unwrap(),
            scale: buf.get(4).unchecked_into_f64(),
        }
    }
}

/// Instant message for sending a function.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MsgFn {
    /// Platform agnostic function pointer converted from fn(), fn(A), fn(A, B) and so on.
    pub(crate) f: u64,

    /// Discriminant of function pointer.
    /// Use [`MsgFnDisc`] for this value.
    pub(crate) disc: u32,
}

#[repr(C)]
union MsgFnCodec {
    src: MsgFn,
    dst: [u32; MsgFn::body_array_len() as usize],
}

impl MsgFn {
    const HEADER: MsgHeader = MsgHeader::FN;

    /// Returns minimum length of [`js_sys::Uint32Array`] for this message's body.
    const fn body_array_len() -> u32 {
        // 4 including padding
        ((mem::size_of::<Self>() + U32_ROUND_UP) / U32_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        // 2, message looks like [header, [body array]].
        const BODY_LEN: u32 = 1;
        HEADER_LEN as u32 + BODY_LEN
    }

    pub(crate) fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        self.write_body(&buf);
        worker.post_message(&buf).unwrap();
    }

    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    fn write_body(&self, buf: &js_sys::Array) {
        let body = js_sys::Uint32Array::new_with_length(Self::body_array_len());
        body.copy_from(&self.encode());
        buf.set(1, body.into());
    }

    pub(crate) fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));

        let body: js_sys::Uint32Array = buf.get(1).unchecked_into();
        let mut buf: [u32; Self::body_array_len() as usize] = [0; Self::body_array_len() as usize];
        body.copy_to(&mut buf);
        Self::decode(buf)
    }

    const fn encode(self) -> [u32; Self::body_array_len() as usize] {
        unsafe { MsgFnCodec { src: self }.dst }
    }

    const fn decode(encoded: [u32; Self::body_array_len() as usize]) -> Self {
        unsafe { MsgFnCodec { dst: encoded }.src }
    }
}

/// Used for [`MsgFn::disc`].
pub(crate) enum MsgFnDisc {
    /// acttey uses from `Begin`.
    Begin = 0x1000_0000,
    UserInit,
}

/// Instant message for requesting run.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MsgRun;

impl MsgRun {
    const HEADER: MsgHeader = MsgHeader::RUN;

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        // 1, header only message
        HEADER_LEN as u32
    }

    pub(crate) fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        worker.post_message(&buf).unwrap();
    }

    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }
}

/// Message for sending current window scale factor (device pixel ratio).
#[derive(Debug, Clone, Copy)]
pub struct MsgWindowScale {
    pub scale: f64,
}

impl MsgWindowScale {
    const HEADER: MsgHeader = MsgHeader::WINDOW_SCALE;

    pub(crate) fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::required_array_len());
        Self::write_header(&buf);
        buf
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    pub(crate) const fn required_array_len() -> u32 {
        // 2
        HEADER_LEN as u32 + 1
    }

    fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    pub(crate) fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        buf.set(1, JsValue::from_f64(self.scale));
    }

    pub(crate) fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));
        Self {
            scale: buf.get(1).unchecked_into_f64(),
        }
    }
}

/// Message for sending current canvas size according to window resize event.
#[derive(Debug, Clone, Copy)]
pub struct MsgCanvasResize {
    pub(crate) handle: CanvasHandle,
    pub width: i16,
    pub height: i16,
}

#[repr(C)]
union MsgCanvasResizeCodec {
    src: MsgCanvasResize,
    dst: [u32; MsgCanvasResize::body_array_len() as usize],
}

impl MsgCanvasResize {
    const HEADER: MsgHeader = MsgHeader::CANVAS_RESIZE;

    pub(crate) fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        buf
    }

    /// Returns minimum length of [`js_sys::Uint32Array`] for this message's body.
    const fn body_array_len() -> u32 {
        // 2
        ((mem::size_of::<Self>() + U32_ROUND_UP) / U32_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        // 2, message looks like [header, [body array]].
        const BODY_LEN: u32 = 1;
        HEADER_LEN as u32 + BODY_LEN
    }

    fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    pub(crate) fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));

        let body = js_sys::Uint32Array::new_with_length(Self::body_array_len());
        body.copy_from(&self.encode());
        buf.set(1, body.into());
    }

    pub(crate) fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));

        let body: js_sys::Uint32Array = buf.get(1).unchecked_into();
        let mut buf: [u32; Self::body_array_len() as usize] = [0; Self::body_array_len() as usize];
        body.copy_to(&mut buf);
        Self::decode(buf)
    }

    const fn encode(self) -> [u32; Self::body_array_len() as usize] {
        unsafe { MsgCanvasResizeCodec { src: self }.dst }
    }

    const fn decode(encoded: [u32; Self::body_array_len() as usize]) -> Self {
        unsafe { MsgCanvasResizeCodec { dst: encoded }.src }
    }
}

/// Message for other mouse messages.
/// Field types may shrink compared to types in [`web_sys::MouseEvent`].
#[derive(Debug, Clone, Copy)]
pub struct MsgMouse {
    pub(crate) handle: CanvasHandle,
    pub button: i8,
    pub client_x: i16,
    pub client_y: i16,
    pub movement_x: i16,
    pub movement_y: i16,
    pub offset_x: i16,
    pub offset_y: i16,
}

#[repr(C)]
union MsgMouseCodec {
    src: MsgMouse,
    dst: [u32; MsgMouse::body_array_len() as usize],
}

impl MsgMouse {
    /// Returns minimum length of [`js_sys::Uint32Array`] for this message's body.
    const fn body_array_len() -> u32 {
        // 5 including padding
        ((mem::size_of::<Self>() + U32_ROUND_UP) / U32_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        // 2, message looks like [header, [body array]].
        const BODY_LEN: u32 = 1;
        HEADER_LEN as u32 + BODY_LEN
    }

    fn write_body(self, buf: &js_sys::Array) {
        let body = js_sys::Uint32Array::new_with_length(Self::body_array_len());
        body.copy_from(&self.encode());
        buf.set(1, body.into());
    }

    fn read_body(buf: &js_sys::Array) -> Self {
        let body: js_sys::Uint32Array = buf.get(1).unchecked_into();
        let mut buf: [u32; Self::body_array_len() as usize] = [0; Self::body_array_len() as usize];
        body.copy_to(&mut buf);
        Self::decode(buf)
    }

    const fn encode(self) -> [u32; Self::body_array_len() as usize] {
        unsafe { MsgMouseCodec { src: self }.dst }
    }

    const fn decode(encoded: [u32; Self::body_array_len() as usize]) -> Self {
        unsafe { MsgMouseCodec { dst: encoded }.src }
    }
}

/// Message for seding mouse move event to main worker.
/// This structure is a wrapper of common mouse message [`MsgMouse`].
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct MsgMouseMove(pub MsgMouse);

impl MsgMouseMove {
    const HEADER: MsgHeader = MsgHeader::MOUSE_MOVE;

    pub(crate) fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        buf
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        // 2, message looks like [header, [body array]].
        MsgMouse::message_array_len()
    }

    fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    pub(crate) fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));

        self.0.write_body(buf);
    }

    pub(crate) fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));

        Self(MsgMouse::read_body(buf))
    }
}

/// Message for sending mouse click event to main worker.
/// This structure is a wrapper of common mouse message [`MsgMouse`].
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct MsgClick(pub MsgMouse);

impl MsgClick {
    const HEADER: MsgHeader = MsgHeader::CLICK;

    pub(crate) fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        buf
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    const fn message_array_len() -> u32 {
        MsgMouse::message_array_len()
    }

    fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    pub(crate) fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));

        self.0.write_body(buf);
    }

    pub(crate) fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgHeader::from_js_value(buf.get(0)));

        Self(MsgMouse::read_body(buf))
    }
}

/// Converts `JsValue` into `Vec<String>`.
fn js_to_vec_string(v: JsValue) -> Vec<String> {
    debug_assert!(v.is_array(), "JsValue is not array");

    let arr: js_sys::Array = v.unchecked_into();
    arr.iter()
        .map(|v| v.as_string().unwrap())
        .collect::<Vec<_>>()
}

/// Converts `Vec<String>` into `JsValue`.
fn vec_string_to_js(v: Vec<String>) -> JsValue {
    let arr = js_sys::Array::new_with_length(v.len() as u32);
    for (i, event) in v.into_iter().enumerate() {
        arr.set(i as u32, event.into());
    }
    JsValue::from(arr)
}
