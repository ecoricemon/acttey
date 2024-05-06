use wasm_bindgen::{JsCast, JsValue, UnwrapThrowExt};
use web_sys::{self, Document, Element, HtmlElement, Navigator, Window};

#[inline]
pub fn window() -> Window {
    web_sys::window().expect_throw(crate::errmsg::WEBSYS_GET_ELEMENT)
}

#[inline]
pub fn device_pixel_ratio_from(window: &Window) -> f64 {
    window.device_pixel_ratio()
}

pub fn device_pixel_ratio() -> f64 {
    device_pixel_ratio_from(&window())
}

#[inline]
pub fn document_from(window: &Window) -> Document {
    window
        .document()
        .expect_throw(crate::errmsg::WEBSYS_GET_ELEMENT)
}

pub fn document() -> Document {
    document_from(&window())
}

#[inline]
pub fn navigator_from(window: &Window) -> Navigator {
    window.navigator()
}

pub fn navigator() -> Navigator {
    navigator_from(&window())
}

#[inline]
pub fn body_from(document: &Document) -> HtmlElement {
    document
        .body()
        .expect_throw(crate::errmsg::WEBSYS_GET_ELEMENT)
}

pub fn body() -> HtmlElement {
    body_from(&document())
}

pub fn get_element_by_id(id: &str) -> Option<Element> {
    document().get_element_by_id(id)
}

#[inline]
pub fn query_selector_from(
    document: &Document,
    selectors: &str,
) -> Result<Option<Element>, JsValue> {
    document.query_selector(selectors)
}

pub fn query_selector(selectors: &str) -> Result<Option<Element>, JsValue> {
    query_selector_from(&document(), selectors)
}

pub fn set_attributes<'a>(
    element: &Element,
    pairs: impl Iterator<Item = (&'a str, &'a str)>,
) -> Result<(), JsValue> {
    for (name, value) in pairs {
        element.set_attribute(name, value)?;
    }
    Ok(())
}

#[inline]
pub fn has_attribute(element: &Element, name: &str) -> bool {
    element.has_attribute(name)
}

pub fn create_element<T: JsCast>(local_name: &str) -> Option<T> {
    let document = document();
    let element = document.create_element(local_name).ok()?;
    let body = document.body()?;
    let element = body.append_child(&element).ok()?;
    element.dyn_into().ok()
}

/// Returns the number of logical processors available to run thread(Web worker) concurrently.
/// It may be lower than the actual number of logical processors depending on browser.
pub fn hardware_concurrency() -> usize {
    if let Some(window) = web_sys::window() {
        let navigator = window.navigator();
        navigator.hardware_concurrency() as usize
    } else {
        let global: web_sys::WorkerGlobalScope = js_sys::global().unchecked_into();
        let navigator = global.navigator();
        navigator.hardware_concurrency() as usize
    }
}

// TODO: Version issue?
// pub fn is_webgpu_available_from(window: &Window) -> bool {
//     !navigator_from(window).gpu().is_undefined()
// }

// TODO: Version issue?
// pub fn is_webgpu_available() -> bool {
//     is_webgpu_available_from(&window())
// }

pub fn cross_origin_isolated() -> bool {
    let prop = "crossOriginIsolated";
    let prop: &JsValue = &prop.into();

    let res = if let Some(window) = web_sys::window() {
        let obj: &JsValue = &window.into();
        js_sys::Reflect::get(obj, prop).unwrap()
    } else {
        let global: web_sys::DedicatedWorkerGlobalScope = js_sys::global().unchecked_into();
        let obj: &JsValue = &global.into();
        js_sys::Reflect::get(obj, prop).unwrap()
    };
    res.as_bool().unwrap()
}
