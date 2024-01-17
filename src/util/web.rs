use wasm_bindgen::{JsCast, JsValue, UnwrapThrowExt};
use web_sys::{window, Document, Element, HtmlElement, Navigator, Window};

pub fn get_window() -> Window {
    window().expect_throw(crate::errmsg::WEBSYS_GET_ELEMENT)
}

pub fn get_document_from(window: &Window) -> Document {
    window
        .document()
        .expect_throw(crate::errmsg::WEBSYS_GET_ELEMENT)
}

pub fn get_document() -> Document {
    get_document_from(&get_window())
}

pub fn get_navigator_from(window: &Window) -> Navigator {
    window.navigator()
}

pub fn get_navigator() -> Navigator {
    get_navigator_from(&get_window())
}

pub fn get_body_from(document: &Document) -> HtmlElement {
    document
        .body()
        .expect_throw(crate::errmsg::WEBSYS_GET_ELEMENT)
}

pub fn get_body() -> HtmlElement {
    get_body_from(&get_document())
}

pub fn get_element_by_id(id: &str) -> Option<Element> {
    let document = get_document();
    document.get_element_by_id(id)
}

pub fn query_selector(selectors: &str) -> Result<Option<Element>, JsValue> {
    let document = get_document();
    document.query_selector(selectors)
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

pub fn create_element<T: JsCast>(local_name: &str) -> Option<T> {
    let document = get_document();
    let element = document.create_element(local_name).ok()?;
    let body = document.body()?;
    let element = body.append_child(&element).ok()?;
    element.dyn_into().ok()
}

pub fn hardware_concurrency_from(navigator: &Navigator) -> usize {
    navigator.hardware_concurrency() as usize
}

/// Returns the number of logical processors available to run thread(Web worker) concurrently.
/// It may be lower than the actual number of logical processors depending on browser.
pub fn hardware_concurrency() -> usize {
    hardware_concurrency_from(&get_navigator())
}

pub fn is_webgpu_available_from(window: &Window) -> bool {
    !get_navigator_from(window).gpu().is_undefined()
}

pub fn is_webgpu_available() -> bool {
    is_webgpu_available_from(&get_window())
}
