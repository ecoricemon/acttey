use super::WorkerId;
use crate::{
    ecs::{
        request::{RequestBuffer, RequestKey},
        system::Invokable,
    },
    render::canvas::CanvasHandle,
};
use std::{fmt::Debug, mem, ptr::NonNull, thread::Thread};
use wasm_bindgen::{JsCast, JsValue};

/// Message event header.  
/// Inner value will be transmuted into f64 and vice versa.
/// You must guarantee that f64 representation is not Nan (or Inf).
/// If so, someone(maybe JS) will change it's bits except Nan bits.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MsgEventHeader(pub u64);

impl MsgEventHeader {
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

    pub const RUN_INNER: u64 = Self::FN_INNER + 1;
    pub const RUN: Self = Self(Self::RUN_INNER);

    /// --- Window message group starts from here --- ///

    pub const WINDOW_SCALE_INNER: u64 = Self::WINDOW | 1;
    pub const WINDOW_SCALE: Self = Self(Self::WINDOW_SCALE_INNER);

    pub const CANVAS_RESIZE_INNER: u64 = Self::WINDOW_SCALE_INNER + 1;
    pub const CANVAS_RESIZE: Self = Self(Self::CANVAS_RESIZE_INNER);

    /// --- Mouse message group starts from here --- ///

    pub const MOUSE_MOVE_INNER: u64 = Self::MOUSE | 1;
    pub const MOUSE_MOVE: Self = Self(Self::MOUSE_MOVE_INNER);

    pub const CLICK_INNER: u64 = Self::MOUSE_MOVE_INNER + 1;
    pub const CLICK: Self = Self(Self::CLICK_INNER);

    #[inline]
    pub fn into_js_value(self) -> JsValue {
        let value = f64::from_bits(self.0);
        debug_assert!(value.is_finite()); // Nan or Inf are now allowed.
        JsValue::from_f64(value)
    }

    #[inline]
    pub(crate) fn from_js_value(value: JsValue) -> Self {
        Self(value.unchecked_into_f64().to_bits())
    }
}

const F64_SIZE: usize = mem::size_of::<f64>();
const F64_ROUND_UP: usize = F64_SIZE - 1;
const HEADER_LEN: usize = (mem::size_of::<MsgEventHeader>() + F64_ROUND_UP) / F64_SIZE;
const U32_SIZE: usize = mem::size_of::<u32>();
const U32_ROUND_UP: usize = U32_SIZE - 1;

/// Instant message for initializing main worker.
#[derive(Debug, Clone)]
pub struct MsgEventInit(pub MsgEventCanvas /* dummy canvas */);

impl MsgEventInit {
    const HEADER: MsgEventHeader = MsgEventHeader::INIT;

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn message_array_len() -> u32 {
        MsgEventCanvas::message_array_len()
    }

    pub fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
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
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));
        Self(MsgEventCanvas::_read_body(buf))
    }
}

/// Instant message for sending offscreen canvas from window to main worker.
#[derive(Debug, Clone)]
pub struct MsgEventCanvas {
    pub element: web_sys::OffscreenCanvas,
    pub handle: CanvasHandle,
    pub selectors: String,
    pub scale: f64,
}

impl MsgEventCanvas {
    const HEADER: MsgEventHeader = MsgEventHeader::CANVAS;

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn message_array_len() -> u32 {
        // 5
        const NUM_OF_FIELDS: u32 = 4;
        HEADER_LEN as u32 + NUM_OF_FIELDS
    }

    pub fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        let tr = self.write_body(&buf);
        worker.post_message_with_transfer(&buf, &tr).unwrap();
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    fn write_body(self, buf: &js_sys::Array) -> js_sys::Array {
        buf.set(1, JsValue::from(self.element.clone()));
        buf.set(
            2,
            JsValue::from_f64(f64::from_bits(self.handle.into_inner() as u64)),
        );
        buf.set(3, JsValue::from_str(&self.selectors));
        buf.set(4, JsValue::from_f64(self.scale));
        let tr = js_sys::Array::new_with_length(1);
        tr.set(0, JsValue::from(self.element));
        tr
    }

    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));
        Self::_read_body(buf)
    }

    #[inline]
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
pub struct MsgEventFn {
    /// Platform agnostic function pointer converted from fn(), fn(A), fn(A, B) and so on.
    pub f: u64,

    /// Discriminant of function pointer.
    /// Use [`MsgEventFnDisc`] for this value.
    pub disc: u32,
}

#[repr(C)]
union MsgEventFnCodec {
    src: MsgEventFn,
    dst: [u32; MsgEventFn::body_array_len() as usize],
}

impl MsgEventFn {
    const HEADER: MsgEventHeader = MsgEventHeader::FN;

    /// Returns minimum length of [`js_sys::Uint32Array`] for this message's body.
    #[inline]
    const fn body_array_len() -> u32 {
        // 4 including padding
        ((mem::size_of::<Self>() + U32_ROUND_UP) / U32_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn message_array_len() -> u32 {
        // 2, header + body array
        HEADER_LEN as u32 + 1
    }

    pub fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        self.write_body(&buf);
        worker.post_message(&buf).unwrap();
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }

    fn write_body(&self, buf: &js_sys::Array) {
        let body = js_sys::Uint32Array::new_with_length(Self::body_array_len());
        body.copy_from(&self.encode());
        buf.set(1, body.into());
    }

    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));

        let body: js_sys::Uint32Array = buf.get(1).unchecked_into();
        let mut buf: [u32; Self::body_array_len() as usize] = [0; Self::body_array_len() as usize];
        body.copy_to(&mut buf);
        Self::decode(buf)
    }

    #[inline]
    const fn encode(self) -> [u32; Self::body_array_len() as usize] {
        unsafe { MsgEventFnCodec { src: self }.dst }
    }

    #[inline]
    const fn decode(encoded: [u32; Self::body_array_len() as usize]) -> Self {
        unsafe { MsgEventFnCodec { dst: encoded }.src }
    }
}

/// Used for [`MsgEventFn::disc`].
pub enum MsgEventFnDisc {
    /// acttey uses from `Begin`.
    Begin = 0x1000_0000,
    UserInit,
}

/// Instant message for requesting run.
#[derive(Debug, Clone, Copy)]
pub struct MsgEventRun;

impl MsgEventRun {
    const HEADER: MsgEventHeader = MsgEventHeader::RUN;

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn message_array_len() -> u32 {
        // 1, header only message
        HEADER_LEN as u32
    }

    pub fn post_to(self, worker: &web_sys::Worker) {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        worker.post_message(&buf).unwrap();
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        buf.set(0, Self::HEADER.into_js_value());
    }
}

/// Message for sending current window scale factor (device pixel ratio).
#[derive(Debug, Clone, Copy)]
pub struct MsgEventWindowScale {
    pub scale: f64,
}

impl MsgEventWindowScale {
    const HEADER: MsgEventHeader = MsgEventHeader::WINDOW_SCALE;

    pub fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::required_array_len());
        Self::write_header(&buf);
        buf
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    pub const fn required_array_len() -> u32 {
        // 2
        HEADER_LEN as u32 + 1
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    pub fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::required_array_len());
        buf.set(1, JsValue::from_f64(self.scale));
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::required_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));
        Self {
            scale: buf.get(1).unchecked_into_f64(),
        }
    }
}

/// Message for sending current canvas size according to window resize event.
#[derive(Debug, Clone, Copy)]
pub struct MsgEventCanvasResize {
    pub handle: CanvasHandle,
    pub width: i16,
    pub height: i16,
}

#[repr(C)]
union MsgEventCanvasResizeCodec {
    src: MsgEventCanvasResize,
    dst: [u32; MsgEventCanvasResize::body_array_len() as usize],
}

impl MsgEventCanvasResize {
    const HEADER: MsgEventHeader = MsgEventHeader::CANVAS_RESIZE;

    pub fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        buf
    }

    /// Returns minimum length of [`js_sys::Uint32Array`] for this message's body.
    #[inline]
    const fn body_array_len() -> u32 {
        // 2
        ((mem::size_of::<Self>() + U32_ROUND_UP) / U32_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn message_array_len() -> u32 {
        // 2, header + body array
        HEADER_LEN as u32 + 1
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    pub fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));

        let body = js_sys::Uint32Array::new_with_length(Self::body_array_len());
        body.copy_from(&self.encode());
        buf.set(1, body.into());
    }

    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));

        let body: js_sys::Uint32Array = buf.get(1).unchecked_into();
        let mut buf: [u32; Self::body_array_len() as usize] = [0; Self::body_array_len() as usize];
        body.copy_to(&mut buf);
        Self::decode(buf)
    }

    #[inline]
    const fn encode(self) -> [u32; Self::body_array_len() as usize] {
        unsafe { MsgEventCanvasResizeCodec { src: self }.dst }
    }

    #[inline]
    const fn decode(encoded: [u32; Self::body_array_len() as usize]) -> Self {
        unsafe { MsgEventCanvasResizeCodec { dst: encoded }.src }
    }
}

/// Message for other mouse messages.
/// Field types may shrink compared to types in [`web_sys::MouseEvent`].
#[derive(Debug, Clone, Copy)]
pub struct MsgEventMouse {
    pub handle: CanvasHandle,
    pub button: i8,
    pub client_x: i16,
    pub client_y: i16,
    pub movement_x: i16,
    pub movement_y: i16,
    pub offset_x: i16,
    pub offset_y: i16,
}

#[repr(C)]
union MsgEventMouseCodec {
    src: MsgEventMouse,
    dst: [u32; MsgEventMouse::body_array_len() as usize],
}

impl MsgEventMouse {
    /// Returns minimum length of [`js_sys::Uint32Array`] for this message's body.
    #[inline]
    const fn body_array_len() -> u32 {
        // 4 including padding
        ((mem::size_of::<Self>() + U32_ROUND_UP) / U32_SIZE) as u32
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn message_array_len() -> u32 {
        // 2, header + body array
        HEADER_LEN as u32 + 1
    }

    #[inline]
    fn write_body(self, buf: &js_sys::Array) {
        let body = js_sys::Uint32Array::new_with_length(Self::body_array_len());
        body.copy_from(&self.encode());
        buf.set(1, body.into());
    }

    #[inline]
    fn read_body(buf: &js_sys::Array) -> Self {
        let body: js_sys::Uint32Array = buf.get(1).unchecked_into();
        let mut buf: [u32; Self::body_array_len() as usize] = [0; Self::body_array_len() as usize];
        body.copy_to(&mut buf);
        Self::decode(buf)
    }

    #[inline]
    const fn encode(self) -> [u32; Self::body_array_len() as usize] {
        unsafe { MsgEventMouseCodec { src: self }.dst }
    }

    #[inline]
    const fn decode(encoded: [u32; Self::body_array_len() as usize]) -> Self {
        unsafe { MsgEventMouseCodec { dst: encoded }.src }
    }
}

/// Message for seding mouse move event to main worker.
/// This structure is a wrapper of common mouse message [`MsgEventMouse`].
#[derive(Debug, Clone, Copy)]
pub struct MsgEventMouseMove(pub MsgEventMouse);

impl MsgEventMouseMove {
    const HEADER: MsgEventHeader = MsgEventHeader::MOUSE_MOVE;

    pub fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        buf
    }

    /// Returns minimum length of [`js_sys::Uint32Array`] for this message's body.
    #[inline]
    const fn body_array_len() -> u32 {
        // 4 including padding
        MsgEventMouse::body_array_len()
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn message_array_len() -> u32 {
        // 2, header + body array
        MsgEventMouse::message_array_len()
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    pub fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));

        self.0.write_body(buf);
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));

        Self(MsgEventMouse::read_body(buf))
    }
}

/// Message for sending mouse click event to main worker.
/// This structure is a wrapper of common mouse message [`MsgEventMouse`].
#[derive(Debug, Clone, Copy)]
pub struct MsgEventClick(pub MsgEventMouse);

impl MsgEventClick {
    const HEADER: MsgEventHeader = MsgEventHeader::CLICK;

    pub fn create_buffer() -> js_sys::Array {
        let buf = js_sys::Array::new_with_length(Self::message_array_len());
        Self::write_header(&buf);
        buf
    }

    /// Returns minimum length of [`js_sys::Uint32Array`] for this message's body.
    #[inline]
    const fn body_array_len() -> u32 {
        // 4 including padding
        MsgEventMouse::body_array_len()
    }

    /// Returns minimum length of [`js_sys::Array`] for this message.
    #[inline]
    const fn message_array_len() -> u32 {
        // 2, header + body array
        MsgEventMouse::message_array_len()
    }

    #[inline]
    fn write_header(buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        buf.set(0, Self::HEADER.into_js_value());
    }

    #[inline]
    pub fn write_body(&self, buf: &js_sys::Array) {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));

        self.0.write_body(buf);
    }

    #[inline]
    pub fn read_body(buf: &js_sys::Array) -> Self {
        debug_assert!(buf.length() >= Self::message_array_len());
        debug_assert_eq!(Self::HEADER, MsgEventHeader::from_js_value(buf.get(0)));

        Self(MsgEventMouse::read_body(buf))
    }
}

/// Channel message that conveyed by channels between main and sub workers.
/// Every sub worker has its own channel when it's spawned.
/// When it comes to procedure of message transportation,
/// worker first is woken up by main worker's call to [`Worker::open`](crate::worker::Worker::open),
/// then the worker starts to listen to the channel and process this channel message.  
/// Don't be confused between *channel message* and *message event*,
/// which is a type of event used on JS.
pub enum ChMsg {
    /// Main worker requests Rust handle from the sub worker.
    ReqHandle,

    /// Main worker sends this to the sub worker in order to notify the end of work at this frame.
    End,

    /// Main worker sends a task to the sub worker.
    Task(NonNull<dyn Invokable>, NonNull<RequestBuffer>),

    /// Sub worker sends this to the main worker as a response of [`ChMsg::ReqHandle`].
    Handle(Thread),

    /// Sub worker notifies the task is done.
    Fin(WorkerId, RequestKey),
}

impl Debug for ChMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const FROM_MAIN: &str = "ChMsg(main -> sub):";
        const FROM_SUB: &str = "ChMsg(sub -> main):";

        match self {
            Self::ReqHandle => write!(f, "{FROM_MAIN} ReqHandle"),
            Self::Task(task_ptr, buf_ptr) => {
                write!(f, "{FROM_MAIN} Task({:?}, {:?})", task_ptr, buf_ptr)
            }
            Self::End => write!(f, "{FROM_MAIN} End"),
            Self::Handle(handle) => write!(f, "{FROM_SUB} Handle({:?})", handle),
            Self::Fin(wid, rkey) => write!(f, "{FROM_SUB} Fin({:?}, {:?})", wid, rkey),
        }
    }
}
