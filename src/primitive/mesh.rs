//! Based on glTF 2.0

use super::vector::Vector;
use std::mem::size_of;

/// Mesh.primitive in glTF.
/// Please see https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#meshes
pub struct Mesh {
    attributes: [Option<MeshAttribute>; MESH_ATTR_NUM],
    attribute_len: usize,
    attribute_num: usize,
    indices: Vec<u32>,
}

impl Mesh {
    #[allow(dead_code)]
    #[inline]
    pub(crate) const fn new() -> Self {
        const VACANT: Option<MeshAttribute> = None;
        Self {
            attributes: [VACANT; MESH_ATTR_NUM],
            attribute_len: 0,
            attribute_num: 0,
            indices: Vec::new(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    #[must_use]
    fn with_attribute(mut self, attr: MeshAttribute) -> Self {
        // All attributes must have the same length.
        if self.attribute_len != 0 {
            assert_eq!(self.attribute_len, attr.len());
        } else {
            self.attribute_len = attr.len();
        }

        self.attribute_num += attr.num();

        let i = attr.index();
        self.attributes[i] = Some(attr);
        self
    }

    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub(crate) fn with_position(self, values: MeshAttributeValues) -> Self {
        self.with_attribute(MeshAttribute::from(MESH_ATTR_IDX_POSITION, values))
    }

    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub(crate) fn with_normal(self, values: MeshAttributeValues) -> Self {
        self.with_attribute(MeshAttribute::from(MESH_ATTR_IDX_NORMAL, values))
    }

    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub(crate) fn with_color(self, values: MeshAttributeValues) -> Self {
        self.with_attribute(MeshAttribute::from(MESH_ATTR_IDX_COLOR, values))
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_attribute(&self, i: usize) -> Option<&MeshAttribute> {
        self.attributes[i].as_ref()
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_attribute_mut(&mut self, i: usize) -> Option<&mut MeshAttribute> {
        self.attributes[i].as_mut()
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_position(&self) -> Option<&MeshAttributeValues> {
        self.get_attribute(MESH_ATTR_IDX_POSITION)
            .map(|attr| attr.as_values(0))
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_position_mut(&mut self) -> Option<&mut MeshAttributeValues> {
        self.get_attribute_mut(MESH_ATTR_IDX_POSITION)
            .map(|attr| attr.as_values_mut(0))
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_normal(&self) -> Option<&MeshAttributeValues> {
        self.get_attribute(MESH_ATTR_IDX_NORMAL)
            .map(|attr| attr.as_values(0))
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_normal_mut(&mut self) -> Option<&mut MeshAttributeValues> {
        self.get_attribute_mut(MESH_ATTR_IDX_NORMAL)
            .map(|attr| attr.as_values_mut(0))
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_color(&self, i: usize) -> Option<&MeshAttributeValues> {
        self.get_attribute(MESH_ATTR_IDX_COLOR)
            .map(|attr| attr.as_values(i))
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_color_mut(&mut self, i: usize) -> Option<&mut MeshAttributeValues> {
        self.get_attribute_mut(MESH_ATTR_IDX_COLOR)
            .map(|attr| attr.as_values_mut(i))
    }

    #[allow(dead_code)]
    // No padding.
    pub(crate) fn create_interleaved_vertex_info(&self) -> InterleavedVertexInfo {
        let vertex_size = self
            .attributes
            .iter()
            .filter_map(|attr| attr.as_ref())
            .fold(0, |acc, attr| {
                acc + (0..attr.num())
                    .map(|inner_i| attr.unit_size(inner_i))
                    .sum::<usize>()
            });

        let mut bytes = vec![0_u8; vertex_size * self.attribute_len];
        let mut wgpu_attributes = Vec::with_capacity(self.attribute_num);

        let mut shader_loc = 0;
        let mut attr_off = 0;
        for attr in self.attributes.iter().filter_map(|attr| attr.as_ref()) {
            for inner_i in 0..attr.num() {
                let chunk_size = attr.unit_size(inner_i);
                let chunks = attr.as_values(inner_i).as_bytes();
                let chunk_iter = chunks.chunks_exact(chunk_size);
                for (vertex_idx, chunk) in chunk_iter.enumerate() {
                    let s = vertex_idx * vertex_size + attr_off;
                    bytes[s..(s + chunk_size)].copy_from_slice(chunk);
                }

                wgpu_attributes.push(wgpu::VertexAttribute {
                    format: attr.format(inner_i),
                    offset: attr_off as wgpu::BufferAddress,
                    shader_location: shader_loc,
                });
                shader_loc += 1;

                attr_off += chunk_size;
            }
        }

        InterleavedVertexInfo {
            bytes,
            wgpu_attributes,
            vertex_size,
        }
    }

    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub(crate) fn with_indices(mut self, indices: Vec<u32>) -> Self {
        self.indices = indices;
        self
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_indices(&self) -> &Vec<u32> {
        &self.indices
    }
}

pub(crate) const MESH_ATTR_IDX_POSITION: usize = 0;
pub(crate) const MESH_ATTR_IDX_NORMAL: usize = 1;
pub(crate) const MESH_ATTR_IDX_COLOR: usize = 2;
pub(crate) const MESH_ATTR_NUM: usize = 3;

pub(crate) enum MeshAttribute {
    Position(MeshAttributeValues),
    Normal(MeshAttributeValues),
    Color(Vec<MeshAttributeValues>), // Color_n, They must have the same length.
}

impl MeshAttribute {
    #[allow(dead_code)]
    #[inline]
    pub(crate) fn index(&self) -> usize {
        match self {
            Self::Position(..) => MESH_ATTR_IDX_POSITION,
            Self::Normal(..) => MESH_ATTR_IDX_NORMAL,
            Self::Color(..) => MESH_ATTR_IDX_COLOR,
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn num(&self) -> usize {
        match self {
            Self::Position(..) => 1,
            Self::Normal(..) => 1,
            Self::Color(vv) => vv.len(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn len(&self) -> usize {
        match self {
            Self::Position(v) => v.len(),
            Self::Normal(v) => v.len(),
            Self::Color(vv) => vv[0].len(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn unit_size(&self, inner_i: usize) -> usize {
        match self {
            Self::Position(v) => v.unit_size(),
            Self::Normal(v) => v.unit_size(),
            Self::Color(vv) => vv[inner_i].unit_size(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn from(i: usize, values: MeshAttributeValues) -> Self {
        match i {
            MESH_ATTR_IDX_POSITION => Self::Position(values),
            MESH_ATTR_IDX_NORMAL => Self::Normal(values),
            MESH_ATTR_IDX_COLOR => Self::Color(vec![values]),
            _ => panic!(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_values(&self, inner_i: usize) -> &MeshAttributeValues {
        match self {
            Self::Position(v) => v,
            Self::Normal(v) => v,
            Self::Color(vv) => &vv[inner_i],
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_values_mut(&mut self, inner_i: usize) -> &mut MeshAttributeValues {
        match self {
            Self::Position(v) => v,
            Self::Normal(v) => v,
            Self::Color(vv) => &mut vv[inner_i],
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn format(&self, inner_i: usize) -> wgpu::VertexFormat {
        match self {
            Self::Position(v) => v.format(),
            Self::Normal(v) => v.format(),
            Self::Color(vv) => vv[inner_i].format(),
        }
    }
}

pub(crate) enum MeshAttributeValues {
    Vec2unorm8(Vec<Vector<u8, 2>>),
    Vec2unorm16(Vec<Vector<u16, 2>>),
    Vec4unorm8(Vec<Vector<u8, 4>>),
    Vec4unorm16(Vec<Vector<u16, 4>>),
    Vec2f32(Vec<Vector<f32, 2>>),
    Vec3f32(Vec<Vector<f32, 3>>),
    Vec4f32(Vec<Vector<f32, 4>>),
}

impl MeshAttributeValues {
    #[allow(dead_code)]
    #[inline]
    pub(crate) fn len(&self) -> usize {
        match self {
            Self::Vec2unorm8(v) => v.len(),
            Self::Vec2unorm16(v) => v.len(),
            Self::Vec4unorm8(v) => v.len(),
            Self::Vec4unorm16(v) => v.len(),
            Self::Vec2f32(v) => v.len(),
            Self::Vec3f32(v) => v.len(),
            Self::Vec4f32(v) => v.len(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn unit_size(&self) -> usize {
        match self {
            Self::Vec2unorm8(..) => 2 * size_of::<u8>(),
            Self::Vec2unorm16(..) => 2 * size_of::<u16>(),
            Self::Vec4unorm8(..) => 4 * size_of::<u8>(),
            Self::Vec4unorm16(..) => 4 * size_of::<u16>(),
            Self::Vec2f32(..) => 2 * size_of::<f32>(),
            Self::Vec3f32(..) => 3 * size_of::<f32>(),
            Self::Vec4f32(..) => 4 * size_of::<f32>(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_bytes(&self) -> &[u8] {
        use bytemuck::cast_slice;
        match self {
            Self::Vec2unorm8(v) => cast_slice(v),
            Self::Vec2unorm16(v) => cast_slice(v),
            Self::Vec4unorm8(v) => cast_slice(v),
            Self::Vec4unorm16(v) => cast_slice(v),
            Self::Vec2f32(v) => cast_slice(v),
            Self::Vec3f32(v) => cast_slice(v),
            Self::Vec4f32(v) => cast_slice(v),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn format(&self) -> wgpu::VertexFormat {
        match self {
            Self::Vec2unorm8(..) => wgpu::VertexFormat::Unorm8x2,
            Self::Vec2unorm16(..) => wgpu::VertexFormat::Unorm16x2,
            Self::Vec4unorm8(..) => wgpu::VertexFormat::Unorm8x4,
            Self::Vec4unorm16(..) => wgpu::VertexFormat::Unorm16x4,
            Self::Vec2f32(..) => wgpu::VertexFormat::Float32x2,
            Self::Vec3f32(..) => wgpu::VertexFormat::Float32x3,
            Self::Vec4f32(..) => wgpu::VertexFormat::Float32x4,
        }
    }
}

macro_rules! impl_from_for_mesh_attribute_values {
    ($x:ty, $y:ident) => {
        impl From<Vec<$x>> for MeshAttributeValues {
            fn from(value: Vec<$x>) -> Self {
                Self::$y(value)
            }
        }

        impl<'a> From<&'a MeshAttributeValues> for &'a Vec<$x> {
            fn from(value: &'a MeshAttributeValues) -> Self {
                match value {
                    MeshAttributeValues::$y(v) => v,
                    _ => panic!(),
                }
            }
        }

        impl<'a> From<&'a mut MeshAttributeValues> for &'a mut Vec<$x> {
            fn from(value: &'a mut MeshAttributeValues) -> Self {
                match value {
                    MeshAttributeValues::$y(v) => v,
                    _ => panic!(),
                }
            }
        }
    };
}

impl_from_for_mesh_attribute_values!(Vector<u8, 2>, Vec2unorm8);
impl_from_for_mesh_attribute_values!(Vector<u16, 2>, Vec2unorm16);
impl_from_for_mesh_attribute_values!(Vector<u8, 4>, Vec4unorm8);
impl_from_for_mesh_attribute_values!(Vector<u16, 4>, Vec4unorm16);
impl_from_for_mesh_attribute_values!(Vector<f32, 2>, Vec2f32);
impl_from_for_mesh_attribute_values!(Vector<f32, 3>, Vec3f32);
impl_from_for_mesh_attribute_values!(Vector<f32, 4>, Vec4f32);

pub(crate) struct InterleavedVertexInfo {
    pub(crate) bytes: Vec<u8>,
    pub(crate) wgpu_attributes: Vec<wgpu::VertexAttribute>,
    pub(crate) vertex_size: usize,
}
