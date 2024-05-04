use super::{
    event::{EventType, EVENT_TYPE_NUM},
    AppError,
};
use crate::{
    ds::vec::OptVec,
    render::canvas::{CanvasHandle, OffCanvas},
    util::web,
};
use std::{
    any::Any,
    collections::HashMap,
    hash::{Hash, Hasher},
    ops::Deref,
    rc::Rc,
};
use wasm_bindgen::prelude::*;

#[derive(Debug)]
pub struct CanvasPack {
    /// Handle to canvas.
    handle_to_canvas: HashMap<CanvasHandle, Rc<Canvas>, ahash::RandomState>,

    /// Selectors to handle.
    selectors_to_handle: HashMap<String, CanvasHandle, ahash::RandomState>,

    /// Monotonically increasing handle number.
    cur_handle: CanvasHandle,

    /// Each canvas has integer handle and that is used as an index of this field.
    proxies: OptVec<[Option<Box<dyn Any>>; EVENT_TYPE_NUM]>,
}

impl CanvasPack {
    pub const DUMMY_SELECTORS: &'static str = "#acttey-dummy-canvas";

    pub fn new() -> Self {
        let mut pack = Self {
            handle_to_canvas: HashMap::default(),
            selectors_to_handle: HashMap::default(),
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
        pack.insert(Self::DUMMY_SELECTORS).unwrap();

        // Handle starts from 1.
        pack.cur_handle = CanvasHandle::window_handle() + 1;

        pack
    }

    pub fn iter(&self) -> impl Iterator<Item = (CanvasHandle, &Rc<Canvas>)> {
        self.handle_to_canvas
            .iter()
            .map(|(handle, canvas)| (*handle, canvas))
    }

    #[inline]
    pub fn handles(&self) -> impl Iterator<Item = CanvasHandle> + '_ {
        self.handle_to_canvas.keys().cloned()
    }

    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &Rc<Canvas>> {
        self.handle_to_canvas.values()
    }

    /// Adds the canvas selected by the given `selectors`.
    pub fn insert(&mut self, selectors: &str) -> Result<Rc<Canvas>, AppError> {
        let canvas = Rc::new(Canvas::new(selectors.as_ref(), self.cur_handle)?);
        if let Some(orphan_handle) = self
            .selectors_to_handle
            .insert(selectors.to_owned(), self.cur_handle)
        {
            self.handle_to_canvas.remove(&orphan_handle);
        }
        self.handle_to_canvas
            .insert(self.cur_handle, Rc::clone(&canvas));
        self.cur_handle += 1;
        Ok(canvas)
    }

    pub fn get_by_selectors(&self, selectors: &str) -> Option<&Rc<Canvas>> {
        if let Some(handle) = self.selectors_to_handle.get(selectors) {
            self.handle_to_canvas.get(handle)
        } else {
            None
        }
    }

    #[inline]
    pub fn get_by_handle(&self, handle: &CanvasHandle) -> Option<&Rc<Canvas>> {
        self.handle_to_canvas.get(handle)
    }

    #[inline]
    pub fn get_dummy(&self) -> &Rc<Canvas> {
        self.get_by_handle(&CanvasHandle::dummy_handle()).unwrap()
    }

    #[inline]
    pub fn selectors_to_handle(&self, selectors: &str) -> Option<CanvasHandle> {
        self.selectors_to_handle.get(selectors).cloned()
    }

    /// Time complexity: O(n)
    pub fn handle_to_selectors(&self, handle: CanvasHandle) -> Option<&str> {
        self.selectors_to_handle
            .iter()
            .find_map(|(selectors, &this_handle)| {
                (this_handle == handle).then_some(selectors.as_ref())
            })
    }

    #[inline]
    pub fn contains_selectors(&self, selectors: &str) -> bool {
        self.get_by_selectors(selectors).is_some()
    }

    #[inline]
    pub fn contains_handle(&self, handle: &CanvasHandle) -> bool {
        self.get_by_handle(handle).is_some()
    }

    /// Clears unused canvases from external and returns the number of removed canvases.
    pub fn clear(&mut self) -> usize {
        let unused = self
            .handle_to_canvas
            .iter()
            .filter_map(|(&handle, canvas)| {
                (!handle.is_dummy_handle() && Rc::strong_count(canvas) == 1).then_some(handle)
            })
            .collect::<Vec<_>>();
        let removed = unused.len();
        for handle in unused {
            self.handle_to_canvas.remove(&handle);
        }
        removed
    }

    pub fn register_proxy(&mut self, handle: CanvasHandle, event: EventType, proxy: Box<dyn Any>) {
        let idx = handle.into_inner() as usize;
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
}

impl Default for CanvasPack {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Canvas {
    /// HTML element.
    element: web_sys::HtmlCanvasElement,

    // ref: https://docs.rs/raw-window-handle/0.5.0/raw_window_handle/struct.WebWindowHandle.html
    /// Integer handle. This is automatically inserted as an element attribute.
    handle: CanvasHandle,
}

impl Canvas {
    /// # Panics
    ///
    /// Panics `handle` is zero, which is reserved for window itself.
    pub fn new(selectors: &str, handle: CanvasHandle) -> Result<Self, AppError> {
        assert!(handle > 0);

        // In the past, we've inerted `data-raw-handle` to the element for wgpu Surface.
        // But now, we insert `data-acttey-handle` for our identifying.
        let element = Self::get_canvas_element(selectors)?;
        if web::has_attribute(&element, "data-acttey-handle") {
            return Err(AppError::DoubleCanvasCreation(selectors.to_owned()));
        }

        web::set_attributes(
            &element,
            [("data-acttey-handle", handle.to_string().as_str())].into_iter(),
        )
        .unwrap();

        Ok(Self { element, handle })
    }

    fn get_canvas_element(selectors: &str) -> Result<web_sys::HtmlCanvasElement, AppError> {
        let window = web::window();
        let document = web::document_from(&window);

        // Gets canvas element.
        let element = web::query_selector_from(&document, selectors)
            .map_err(|_| AppError::CanvasQueryError(selectors.to_owned()))?;
        let element = element.ok_or(AppError::CanvasQueryError(selectors.to_owned()))?;
        let canvas = element
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .map_err(|_| AppError::CanvasQueryError(selectors.to_owned()))?;

        // Adjusts CSS perspective width and height to the current scaled size.
        let scale = web::device_pixel_ratio_from(&window);
        let width = (canvas.client_width() as f64 * scale) as u32;
        let height = (canvas.client_height() as f64 * scale) as u32;
        canvas.set_width(width);
        canvas.set_height(height);
        Ok(canvas)
    }

    #[inline]
    pub fn handle(&self) -> CanvasHandle {
        self.handle
    }

    #[inline]
    pub fn is_dummy(&self) -> bool {
        self.handle.is_dummy_handle()
    }

    #[inline]
    pub fn is_window(&self) -> bool {
        self.handle.is_window_handle()
    }

    pub fn transfer_control_to_offscreen(&self) -> Result<OffCanvas, AppError> {
        let offscreen = self
            .element
            .transfer_control_to_offscreen()
            .map_err(|_| AppError::DoubleOffscreenCanvas(self.handle))?;
        Ok(OffCanvas::new(offscreen, self.handle))
    }
}

impl Deref for Canvas {
    type Target = web_sys::HtmlCanvasElement;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

impl Hash for Canvas {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.handle.hash(state);
    }
}

impl PartialEq for Canvas {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}
