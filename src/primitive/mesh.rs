//! Based on glTF 2.0

use crate::primitive::vector::Vector;
use std::mem::size_of;

/// Mesh.primitive in glTF.
/// Please see https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#meshes
#[derive(Clone)]
pub struct Mesh {
    attributes: [Option<MeshAttribute>; MESH_ATTR_NUM],
    attribute_len: usize,
    attribute_num: usize,
    indices: MeshIndices,
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
            indices: MeshIndices::new(),
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

        let mut vertex_bytes = vec![0_u8; vertex_size * self.attribute_len];
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
                    vertex_bytes[s..(s + chunk_size)].copy_from_slice(chunk);
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
            vertex_bytes,
            wgpu_attributes,
            vertex_size,
            vertex_num: self.attribute_len,
            index_bytes: self.indices.clone().into(),
            index_format: self.indices.format(),
            index_num: self.indices.len(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub(crate) fn with_indices(mut self, indices: MeshIndices) -> Self {
        self.indices = indices;
        self
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_indices(&self) -> &MeshIndices {
        &self.indices
    }
}

pub(crate) const MESH_ATTR_IDX_POSITION: usize = 0;
pub(crate) const MESH_ATTR_IDX_NORMAL: usize = 1;
pub(crate) const MESH_ATTR_IDX_TANGENT: usize = 2; // Not implemented.
pub(crate) const MESH_ATTR_IDX_TEXCOORD: usize = 3; // Not implemented.
pub(crate) const MESH_ATTR_IDX_COLOR: usize = 4;
pub(crate) const MESH_ATTR_IDX_JOINTS: usize = 5; // Not implemented.
pub(crate) const MESH_ATTR_IDX_WEIGHTS: usize = 6; // Not implemented.
pub(crate) const MESH_ATTR_NUM: usize = 7;

#[derive(Clone)]
pub(crate) enum MeshAttribute {
    Position(MeshAttributeValues),
    Normal(MeshAttributeValues),
    Tangent(MeshAttributeValues),
    TexCoord(Vec<MeshAttributeValues>), // TEXTCOORD_n, They must have the same length.
    Color(Vec<MeshAttributeValues>),    // COLOR_n, They must have the same length.
    Joints(Vec<MeshAttributeValues>),   // JOINTS_n, They must have the same length.
    Weights(Vec<MeshAttributeValues>),  // WEIGHTS_n, They must have the same length.
}

impl MeshAttribute {
    #[allow(dead_code)]
    #[inline]
    pub(crate) fn index(&self) -> usize {
        match self {
            Self::Position(..) => MESH_ATTR_IDX_POSITION,
            Self::Normal(..) => MESH_ATTR_IDX_NORMAL,
            Self::Tangent(..) => MESH_ATTR_IDX_TANGENT,
            Self::TexCoord(..) => MESH_ATTR_IDX_TEXCOORD,
            Self::Color(..) => MESH_ATTR_IDX_COLOR,
            Self::Joints(..) => MESH_ATTR_IDX_JOINTS,
            Self::Weights(..) => MESH_ATTR_IDX_WEIGHTS,
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn num(&self) -> usize {
        match self {
            Self::Position(..) => 1,
            Self::Normal(..) => 1,
            Self::Tangent(..) => 1,
            Self::TexCoord(vv) => vv.len(),
            Self::Color(vv) => vv.len(),
            Self::Joints(vv) => vv.len(),
            Self::Weights(vv) => vv.len(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn len(&self) -> usize {
        match self {
            Self::Position(v) => v.len(),
            Self::Normal(v) => v.len(),
            Self::Tangent(v) => v.len(),
            Self::TexCoord(vv) => vv[0].len(),
            Self::Color(vv) => vv[0].len(),
            Self::Joints(vv) => vv[0].len(),
            Self::Weights(vv) => vv[0].len(),
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
            Self::Tangent(v) => v.unit_size(),
            Self::TexCoord(vv) => vv[inner_i].unit_size(),
            Self::Color(vv) => vv[inner_i].unit_size(),
            Self::Joints(vv) => vv[inner_i].unit_size(),
            Self::Weights(vv) => vv[inner_i].unit_size(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn from(i: usize, values: MeshAttributeValues) -> Self {
        match i {
            MESH_ATTR_IDX_POSITION => Self::Position(values),
            MESH_ATTR_IDX_NORMAL => Self::Normal(values),
            MESH_ATTR_IDX_TANGENT => Self::Tangent(values),
            MESH_ATTR_IDX_TEXCOORD => Self::TexCoord(vec![values]),
            MESH_ATTR_IDX_COLOR => Self::Color(vec![values]),
            MESH_ATTR_IDX_JOINTS => Self::Joints(vec![values]),
            MESH_ATTR_IDX_WEIGHTS => Self::Weights(vec![values]),
            _ => panic!(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_values(&self, inner_i: usize) -> &MeshAttributeValues {
        match self {
            Self::Position(v) => v,
            Self::Normal(v) => v,
            Self::Tangent(v) => v,
            Self::TexCoord(vv) => &vv[inner_i],
            Self::Color(vv) => &vv[inner_i],
            Self::Joints(vv) => &vv[inner_i],
            Self::Weights(vv) => &vv[inner_i],
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_values_mut(&mut self, inner_i: usize) -> &mut MeshAttributeValues {
        match self {
            Self::Position(v) => v,
            Self::Normal(v) => v,
            Self::Tangent(v) => v,
            Self::TexCoord(vv) => &mut vv[inner_i],
            Self::Color(vv) => &mut vv[inner_i],
            Self::Joints(vv) => &mut vv[inner_i],
            Self::Weights(vv) => &mut vv[inner_i],
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn format(&self, inner_i: usize) -> wgpu::VertexFormat {
        match self {
            Self::Position(v) => v.format(),
            Self::Normal(v) => v.format(),
            Self::Tangent(v) => v.format(),
            Self::TexCoord(vv) => vv[inner_i].format(),
            Self::Color(vv) => vv[inner_i].format(),
            Self::Joints(vv) => vv[inner_i].format(),
            Self::Weights(vv) => vv[inner_i].format(),
        }
    }
}

#[derive(Clone)]
pub(crate) enum MeshAttributeValues {
    VecUint8x4(Vec<Vector<u8, 4>>),
    VecUnorm8x2(Vec<Vector<u8, 2>>),
    VecUnorm8x4(Vec<Vector<u8, 4>>),
    VecUint16x4(Vec<Vector<u16, 4>>),
    VecUnorm16x2(Vec<Vector<u16, 2>>),
    VecUnorm16x4(Vec<Vector<u16, 4>>),
    VecFloat32x2(Vec<Vector<f32, 2>>),
    VecFloat32x3(Vec<Vector<f32, 3>>),
    VecFloat32x4(Vec<Vector<f32, 4>>),
}

impl MeshAttributeValues {
    #[allow(dead_code)]
    #[inline]
    pub(crate) fn from_vec_uint8x4(v: Vec<Vector<u8, 4>>) -> Self {
        Self::VecUint8x4(v)
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_vec_uint8x4(&self) -> &Vec<Vector<u8, 4>> {
        match self {
            Self::VecUint8x4(v) => v,
            _ => panic!(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_vec_mut_uint8x4(&mut self) -> &mut Vec<Vector<u8, 4>> {
        match self {
            Self::VecUint8x4(v) => v,
            _ => panic!(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn from_vec_uint16x4(v: Vec<Vector<u16, 4>>) -> Self {
        Self::VecUint16x4(v)
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_vec_uint16x4(&self) -> &Vec<Vector<u16, 4>> {
        match self {
            Self::VecUint16x4(v) => v,
            _ => panic!(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_vec_mut_uint16x4(&mut self) -> &mut Vec<Vector<u16, 4>> {
        match self {
            Self::VecUint16x4(v) => v,
            _ => panic!(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn len(&self) -> usize {
        match self {
            Self::VecUint8x4(v) => v.len(),
            Self::VecUnorm8x2(v) => v.len(),
            Self::VecUnorm8x4(v) => v.len(),
            Self::VecUint16x4(v) => v.len(),
            Self::VecUnorm16x2(v) => v.len(),
            Self::VecUnorm16x4(v) => v.len(),
            Self::VecFloat32x2(v) => v.len(),
            Self::VecFloat32x3(v) => v.len(),
            Self::VecFloat32x4(v) => v.len(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn unit_size(&self) -> usize {
        match self {
            Self::VecUint8x4(..) => size_of::<Vector<u8, 4>>(),
            Self::VecUnorm8x2(..) => size_of::<Vector<u8, 2>>(),
            Self::VecUnorm8x4(..) => size_of::<Vector<u8, 4>>(),
            Self::VecUint16x4(..) => size_of::<Vector<u16, 4>>(),
            Self::VecUnorm16x2(..) => size_of::<Vector<u16, 2>>(),
            Self::VecUnorm16x4(..) => size_of::<Vector<u16, 4>>(),
            Self::VecFloat32x2(..) => size_of::<Vector<f32, 2>>(),
            Self::VecFloat32x3(..) => size_of::<Vector<f32, 3>>(),
            Self::VecFloat32x4(..) => size_of::<Vector<f32, 4>>(),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn as_bytes(&self) -> &[u8] {
        use bytemuck::cast_slice;
        match self {
            Self::VecUint8x4(v) => cast_slice(v),
            Self::VecUnorm8x2(v) => cast_slice(v),
            Self::VecUnorm8x4(v) => cast_slice(v),
            Self::VecUint16x4(v) => cast_slice(v),
            Self::VecUnorm16x2(v) => cast_slice(v),
            Self::VecUnorm16x4(v) => cast_slice(v),
            Self::VecFloat32x2(v) => cast_slice(v),
            Self::VecFloat32x3(v) => cast_slice(v),
            Self::VecFloat32x4(v) => cast_slice(v),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn format(&self) -> wgpu::VertexFormat {
        match self {
            Self::VecUint8x4(..) => wgpu::VertexFormat::Uint8x4,
            Self::VecUnorm8x2(..) => wgpu::VertexFormat::Unorm8x2,
            Self::VecUnorm8x4(..) => wgpu::VertexFormat::Unorm8x4,
            Self::VecUint16x4(..) => wgpu::VertexFormat::Uint16x4,
            Self::VecUnorm16x2(..) => wgpu::VertexFormat::Unorm16x2,
            Self::VecUnorm16x4(..) => wgpu::VertexFormat::Unorm16x4,
            Self::VecFloat32x2(..) => wgpu::VertexFormat::Float32x2,
            Self::VecFloat32x3(..) => wgpu::VertexFormat::Float32x3,
            Self::VecFloat32x4(..) => wgpu::VertexFormat::Float32x4,
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

// Use these instead of From::from()
// MeshAttributeValues::from_vec_uint8x4()
// MeshAttributeValues::as_vec_uint8x4()
// MeshAttributeValues::as_vec_mut_uint8x4()
// impl_from_for_mesh_attribute_values!(Vector<u8, 4>, VecUint8x4);
impl_from_for_mesh_attribute_values!(Vector<u8, 2>, VecUnorm8x2);
impl_from_for_mesh_attribute_values!(Vector<u8, 4>, VecUnorm8x4);
// Use these instead of From::from()
// MeshAttributeValues::from_vec_uint16x4()
// MeshAttributeValues::as_vec_uint16x4()
// MeshAttributeValues::as_vec_mut_uint16x4()
// impl_from_for_mesh_attribute_values!(Vector<u16, 4>, VecUint16x4);
impl_from_for_mesh_attribute_values!(Vector<u16, 2>, VecUnorm16x2);
impl_from_for_mesh_attribute_values!(Vector<u16, 4>, VecUnorm16x4);
impl_from_for_mesh_attribute_values!(Vector<f32, 2>, VecFloat32x2);
impl_from_for_mesh_attribute_values!(Vector<f32, 3>, VecFloat32x3);
impl_from_for_mesh_attribute_values!(Vector<f32, 4>, VecFloat32x4);

pub(crate) struct InterleavedVertexInfo {
    pub(crate) vertex_bytes: Vec<u8>,
    pub(crate) wgpu_attributes: Vec<wgpu::VertexAttribute>,
    pub(crate) vertex_size: usize,
    pub(crate) vertex_num: usize,
    pub(crate) index_bytes: Vec<u8>,
    pub(crate) index_format: wgpu::IndexFormat,
    pub(crate) index_num: usize,
}

impl From<Mesh> for InterleavedVertexInfo {
    fn from(value: Mesh) -> Self {
        value.create_interleaved_vertex_info()
    }
}

#[derive(Clone)]
pub(crate) enum MeshIndices {
    U16(Vec<u16>),
    U32(Vec<u32>),
}

impl MeshIndices {
    pub(crate) const fn new() -> MeshIndices {
        Self::U16(vec![])
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        match self {
            Self::U16(v) => v.len(),
            Self::U32(v) => v.len(),
        }
    }

    pub(crate) fn format(&self) -> wgpu::IndexFormat {
        match self {
            Self::U16(..) => wgpu::IndexFormat::Uint16,
            Self::U32(..) => wgpu::IndexFormat::Uint32,
        }
    }
}

impl From<Vec<u16>> for MeshIndices {
    fn from(value: Vec<u16>) -> Self {
        MeshIndices::U16(value)
    }
}

impl From<Vec<u32>> for MeshIndices {
    fn from(value: Vec<u32>) -> Self {
        MeshIndices::U32(value)
    }
}

impl From<Vec<i32>> for MeshIndices {
    fn from(value: Vec<i32>) -> Self {
        MeshIndices::U32(bytemuck::cast_vec(value))
    }
}

impl From<MeshIndices> for Vec<u8> {
    fn from(value: MeshIndices) -> Self {
        match value {
            MeshIndices::U16(v) => {
                let casted: &[u8] = bytemuck::cast_slice(&v[..]);
                Vec::from(casted)
            }
            MeshIndices::U32(v) => {
                let casted: &[u8] = bytemuck::cast_slice(&v[..]);
                Vec::from(casted)
            }
        }
    }
}

impl<'a> From<&'a MeshIndices> for &'a [u8] {
    fn from(value: &'a MeshIndices) -> Self {
        match value {
            MeshIndices::U16(v) => bytemuck::cast_slice(v),
            MeshIndices::U32(v) => bytemuck::cast_slice(v),
        }
    }
}
