use super::buffer::res::BufferRc;
use crate::util::key::ObjectKey;
use std::{rc::Rc, sync::Arc};

/// A descriptor for binding buffers to a bind group.
/// This descriptor can help you to add bind group easily
/// but it's hard to reuse resources such as builders.
/// `layout_label`, `group_label`, and `bufs` are mandatory.
pub(crate) struct BufferBindDesc<'a> {
    pub(crate) label: Arc<str>,

    /// *Required field*  
    pub(crate) bufs: &'a [&'a BufferRc],

    /// *Required field*  
    /// Single item size in bytes.
    pub(crate) item_sizes: &'a [u64],

    /// *Optional field*  
    /// Default is Uniform.
    pub(crate) bind_type: wgpu::BufferBindingType,

    /// *Optional field*  
    /// Default is empty.
    /// But if it's left as empty, it's automatically evaluated something like 0, 1, 2, ...
    pub(crate) bindings: &'a [u32],

    /// *Optional field*  
    /// Default is empty.
    /// But if it's left as empty, it's automatically evaluated as VERTEX_FRAGMENT.
    pub(crate) viss: &'a [wgpu::ShaderStages],
}

impl<'a> BufferBindDesc<'a> {
    /// Returns the length of `bufs`.
    pub(crate) fn len(&self) -> usize {
        self.bufs.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Gets a recorded binding or returns default binding number.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub(crate) fn get_binding_or_default(&self, index: usize) -> u32 {
        assert!(index < self.bufs.len());
        if self.bindings.is_empty() {
            index as u32
        } else {
            self.bindings[index]
        }
    }

    /// Gets a recorded visibility or returns default visibility.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub(crate) fn get_visibility_or_default(&self, index: usize) -> wgpu::ShaderStages {
        assert!(index < self.bufs.len());
        if self.viss.is_empty() {
            wgpu::ShaderStages::VERTEX_FRAGMENT
        } else {
            self.viss[index]
        }
    }
}

impl<'a> Default for BufferBindDesc<'a> {
    fn default() -> Self {
        Self {
            label: Arc::clone(ObjectKey::default().label()),
            bufs: &[],
            item_sizes: &[],
            bind_type: wgpu::BufferBindingType::Uniform,
            bindings: &[],
            viss: &[],
        }
    }
}
