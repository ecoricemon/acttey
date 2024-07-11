use crate::{
    common::AppHasher,
    msg::{EventType, EVENT_TYPE_NUM},
    util::web,
};
use my_ecs::{ds::prelude::*, util::prelude::*};
use std::{
    any::Any,
    cmp::{Eq, Ordering, PartialEq, PartialOrd},
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
    ops::{Add, AddAssign, Deref},
    rc::Rc,
};
use wasm_bindgen::prelude::*;

#[derive(Debug)]
pub(crate) struct WinCanvasPack {
    /// [`CanvasPack`] that contains [`WinCanvas`] only.
    canvases: CanvasPack<WinCanvas>,

    /// Monotonically increasing handle number.
    cur_handle: CanvasHandle,

    /// Each canvas has integer handle and that is used as an index of this field.
    proxies: OptVec<[Option<Box<dyn Any>>; EVENT_TYPE_NUM], AppHasher>,
}

impl WinCanvasPack {
    pub(crate) const DUMMY_SEL: &'static str = "#acttey-dummy-canvas";

    pub(crate) fn new() -> Self {
        let mut inst = Self {
            canvases: CanvasPack::new(),
            cur_handle: CanvasHandle::dummy_handle(),
            proxies: OptVec::new(),
        };

        // Creates dummy canvas to make compatible wgpu::Adapter.
        // TODO: dummy canvas is used for generating surface compatible wgpu adapter.
        // is it really working as expected?
        let dummy_id = "acttey-dummy-canvas";
        let dummy_canvas: web_sys::HtmlCanvasElement =
            web::create_element("canvas").expect_throw(crate::errmsg::WEBSYS_ADD_ELEMENT);
        web::set_attributes(
            &dummy_canvas,
            [("id", dummy_id), ("hidden", "")].into_iter(),
        )
        .unwrap();

        // Adds dummy canvas.
        inst.insert(Self::DUMMY_SEL.to_owned());

        // Handle starts from 1.
        inst.cur_handle = CanvasHandle::window_handle() + 1;

        inst
    }

    pub(crate) fn get_dummy(&self) -> &Rc<WinCanvas> {
        self.get_by_handle(&CanvasHandle::dummy_handle()).unwrap()
    }

    pub(crate) fn register_proxy(
        &mut self,
        handle: CanvasHandle,
        event: EventType,
        proxy: Box<dyn Any>,
    ) {
        let idx = handle.unwrap() as usize;
        let ev_idx = event as isize as usize;
        if self.proxies.len() <= idx {
            let mut value = [None, None, None, None];
            value[ev_idx] = Some(proxy);
            self.proxies.extend_set(idx, value);
        } else {
            // Safety: `idx` has been checked.
            let value = unsafe { self.proxies.get_unchecked_mut(idx) };
            value[ev_idx] = Some(proxy);
        }
    }

    pub(crate) fn insert(&mut self, sel: String) {
        let canvas = WinCanvas::new(&sel, self.cur_handle);
        self.canvases.insert(sel, self.cur_handle, Rc::new(canvas));
        self.cur_handle += 1;
    }
}

impl Deref for WinCanvasPack {
    type Target = CanvasPack<WinCanvas>;

    fn deref(&self) -> &Self::Target {
        &self.canvases
    }
}

#[derive(Debug)]
pub(crate) struct CanvasPack<T> {
    /// Handle to canvas.
    handle_to_canvas: HashMap<CanvasHandle, Rc<T>, AppHasher>,

    /// Selectors to handle.
    sel_to_handle: HashMap<String, CanvasHandle, AppHasher>,
}

impl<T> CanvasPack<T> {
    pub(crate) fn new() -> Self {
        Self {
            handle_to_canvas: HashMap::default(),
            sel_to_handle: HashMap::default(),
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (CanvasHandle, &Rc<T>)> {
        self.handle_to_canvas
            .iter()
            .map(|(handle, canvas)| (*handle, canvas))
    }

    pub(crate) fn handles(&self) -> impl Iterator<Item = CanvasHandle> + '_ {
        self.handle_to_canvas.keys().cloned()
    }

    pub(crate) fn canvases(&self) -> impl Iterator<Item = &Rc<T>> {
        self.handle_to_canvas.values()
    }

    pub(crate) fn get_by_selectors(&self, sel: &str) -> Option<&Rc<T>> {
        if let Some(handle) = self.sel_to_handle.get(sel) {
            self.handle_to_canvas.get(handle)
        } else {
            None
        }
    }

    pub(crate) fn get_by_handle(&self, handle: &CanvasHandle) -> Option<&Rc<T>> {
        self.handle_to_canvas.get(handle)
    }

    pub(crate) fn selectors_to_handle(&self, sel: &str) -> Option<CanvasHandle> {
        self.sel_to_handle.get(sel).cloned()
    }

    /// Time complexity: O(n)
    pub(crate) fn handle_to_selectors(&self, handle: CanvasHandle) -> Option<&str> {
        self.sel_to_handle
            .iter()
            .find_map(|(sel, &this_handle)| (this_handle == handle).then_some(sel.as_ref()))
    }

    pub(crate) fn contains_selectors(&self, sel: &str) -> bool {
        self.get_by_selectors(sel).is_some()
    }

    pub(crate) fn contains_handle(&self, handle: &CanvasHandle) -> bool {
        self.get_by_handle(handle).is_some()
    }

    pub(crate) fn insert(&mut self, sel: String, handle: CanvasHandle, canvas: Rc<T>) {
        // Disallows duplicate selectors and handle.
        assert!(!self.handle_to_canvas.contains_key(&handle));
        assert!(!self.sel_to_handle.contains_key(&sel));

        self.handle_to_canvas.insert(handle, canvas);
        self.sel_to_handle.insert(sel, handle);
    }

    pub(crate) fn strong_count(&self, sel: &str) -> Option<usize> {
        let handle = self.sel_to_handle.get(sel)?;
        let canvas = self.handle_to_canvas.get(handle)?;
        Some(Rc::strong_count(canvas))
    }

    /// Removes canvas and returns it if and only if it's currently not referenced.
    /// You can check it out by calling to [`Self::strong_count`] in advance.
    ///
    /// # Panics
    ///
    /// Panics if reference count is greater than 1 (debug mode only).
    pub(crate) fn remove(&mut self, sel: &str) -> Option<Rc<T>> {
        let handle = self.sel_to_handle.remove(sel)?;
        let old = self.handle_to_canvas.remove(&handle);
        if let Some(old) = old.as_ref() {
            debug_assert!(Rc::strong_count(old) == 1, "can't remove referencing item");
        }
        old
    }
}

#[derive(Debug, Clone)]
pub(crate) struct OffCanvas {
    element: web_sys::OffscreenCanvas,

    /// Unique handle.
    handle: CanvasHandle,
}

impl OffCanvas {
    pub(crate) const fn new(element: web_sys::OffscreenCanvas, handle: CanvasHandle) -> Self {
        Self { element, handle }
    }

    pub(crate) fn destructure(self) -> (web_sys::OffscreenCanvas, CanvasHandle) {
        (self.element, self.handle)
    }

    pub(crate) const fn handle(&self) -> CanvasHandle {
        self.handle
    }
}

impl Deref for OffCanvas {
    type Target = web_sys::OffscreenCanvas;

    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl PartialEq for OffCanvas {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl Eq for OffCanvas {}

impl Hash for OffCanvas {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.handle.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct WinCanvas {
    /// HTML element.
    element: web_sys::HtmlCanvasElement,

    // ref: https://docs.rs/raw-window-handle/0.5.0/raw_window_handle/struct.WebWindowHandle.html
    /// Integer handle. This is automatically inserted as an element attribute.
    handle: CanvasHandle,
}

impl WinCanvas {
    /// # Panics
    ///
    /// Panics `handle` is zero, which is reserved for window itself.
    pub(crate) fn new(sel: &str, handle: CanvasHandle) -> Self {
        fn get_canvas_element(sel: &str) -> web_sys::HtmlCanvasElement {
            let window = web::window();
            let document = web::document_from(&window);

            // Gets canvas element.
            let errmsg = debug_format!("failed to find canvas using {sel}");
            let element = web::query_selector_from(&document, sel).expect(&errmsg); /* Syntax error? */
            let element = element.expect(&errmsg);
            let canvas = element
                .dyn_into::<web_sys::HtmlCanvasElement>()
                .expect(&errmsg);

            // Adjusts CSS perspective width and height to the current scaled size.
            let scale = web::device_pixel_ratio_from(&window);
            let width = (canvas.client_width() as f64 * scale) as u32;
            let height = (canvas.client_height() as f64 * scale) as u32;
            canvas.set_width(width);
            canvas.set_height(height);
            canvas
        }

        assert!(handle > 0);

        // In the past, we inserted `data-raw-handle` to the element for wgpu Surface.
        // But now, we insert `data-acttey-handle` for our identifying.
        let element = get_canvas_element(sel);
        assert!(
            !web::has_attribute(&element, "data-acttey-handle"),
            "canvas found by {sel} had 'data-acttey-handle' already"
        );

        web::set_attributes(
            &element,
            [("data-acttey-handle", handle.to_string().as_str())].into_iter(),
        )
        .unwrap();

        Self { element, handle }
    }

    pub(crate) fn handle(&self) -> CanvasHandle {
        self.handle
    }

    pub(crate) fn is_dummy(&self) -> bool {
        self.handle.is_dummy_handle()
    }

    pub(crate) fn is_window(&self) -> bool {
        self.handle.is_window_handle()
    }

    pub(crate) fn transfer_control_to_offscreen(&self) -> OffCanvas {
        let errmsg = debug_format!("failed to transfer control to offscreen from {:?}", self);
        let element = self.element.transfer_control_to_offscreen().expect(&errmsg);
        OffCanvas::new(element, self.handle)
    }
}

impl Deref for WinCanvas {
    type Target = web_sys::HtmlCanvasElement;

    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl PartialEq for WinCanvas {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl Eq for WinCanvas {}

impl Hash for WinCanvas {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.handle.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct CanvasHandle(u32);

impl CanvasHandle {
    const DUMMY: Self = Self::new(u32::MAX - 1);
    const WINDOW: Self = Self::new(0);

    pub(crate) const fn new(handle: u32) -> Self {
        Self(handle)
    }

    pub(crate) const fn dummy_handle() -> Self {
        Self::DUMMY
    }

    pub(crate) const fn window_handle() -> Self {
        Self::WINDOW
    }

    pub(crate) fn is_dummy_handle(&self) -> bool {
        *self == Self::dummy_handle()
    }

    pub(crate) fn is_window_handle(&self) -> bool {
        *self == Self::window_handle()
    }

    pub(crate) const fn unwrap(self) -> u32 {
        self.0
    }
}

impl Display for CanvasHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add for CanvasHandle {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.0 + rhs.0)
    }
}

impl Add<u32> for CanvasHandle {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        Self::new(self.0 + rhs)
    }
}

impl AddAssign for CanvasHandle {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl AddAssign<u32> for CanvasHandle {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl PartialEq<u32> for CanvasHandle {
    fn eq(&self, other: &u32) -> bool {
        &self.0 == other
    }
}

impl PartialOrd<u32> for CanvasHandle {
    fn partial_cmp(&self, other: &u32) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}
