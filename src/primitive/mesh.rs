//! Based on glTF 2.0

use crate::{
    ds::generational::{GenIndex, GenIndexRc, GenVecRc, LabelOrGenIndexRc},
    impl_from_for_enum,
    primitive::{constant::colors, vector::Vector, Color},
    render::buffer::MultiBufferData,
    util::{AorB, AsMultiBytes},
};
use ahash::AHashMap;
use smallvec::SmallVec;
use std::{borrow::Borrow, mem::size_of, rc::Rc};

/// Geometry variants.
#[derive(Debug)]
pub enum GeometryVar {
    /// Geometry with seperate attributes.
    Sep(Box<Geometry>),
    /// Geometry with interleaved attributes.
    Int(InterleavedGeometry),
    /// GPU buffer and geometry with interleaved attributes.
    BufInt(MultiBufferData<InterleavedGeometry, 2>),
}

impl GeometryVar {
    pub fn sort(&mut self) {
        if let Self::Sep(geo) = self {
            geo.sort();
        }
    }

    pub fn create_interleaved(&self) -> Option<InterleavedGeometry> {
        if let Self::Sep(geo) = self {
            Some(InterleavedGeometry::from(&**geo))
        } else {
            None
        }
    }

    /// Makes this interleaved attributes variant.
    /// If caller gave `int_geo`, it will be used instead of generating it.
    pub fn into_interleaved(&mut self, int_geo: Option<InterleavedGeometry>) {
        if let Self::Sep(geo) = self {
            *self = Self::Int(int_geo.unwrap_or(InterleavedGeometry::from(&**geo)));
        }
    }

    /// Makes this buffer and interleaved attributes variant.
    /// If caller gave `int_geo`, it will be used instead of generating it.
    /// If this was already interleaved and `int_geo` was given, `int_geo` will be used anyway.
    /// Given value has priority always.
    pub fn into_buffer_interleaved(
        &mut self,
        int_geo: Option<InterleavedGeometry>,
        bufs: [Rc<wgpu::Buffer>; 2],
    ) {
        match self {
            Self::Sep(geo) => {
                *self = Self::BufInt(MultiBufferData::new(
                    bufs,
                    int_geo.unwrap_or(InterleavedGeometry::from(&**geo)),
                ));
            }
            Self::Int(old_int_geo) => {
                *self = Self::BufInt(MultiBufferData::new(
                    bufs,
                    int_geo.unwrap_or(old_int_geo.clone()),
                ));
            }
            _ => (),
        }
    }

    pub fn as_interleaved(&self) -> Option<&InterleavedGeometry> {
        match self {
            Self::Int(int_geo) => Some(int_geo),
            Self::BufInt(buf_int_geo) => Some(&buf_int_geo.data),
            _ => None,
        }
    }
}

impl_from_for_enum!(GeometryVar, Sep, Box<Geometry>);
impl_from_for_enum!(GeometryVar, Int, InterleavedGeometry);
impl_from_for_enum!(GeometryVar, BufInt, MultiBufferData<InterleavedGeometry, 2>);

/// Top struct of geometries and materials.
#[derive(Debug)]
pub struct MeshResource {
    // TODO: call clean for GenVecRc.
    /// Geometries.
    geos: GenVecRc<GeometryVar>,
    /// Materials.
    mats: GenVecRc<Material>,
    /// Geometry name to its index.
    /// If users put in a geometry with its name, Rc is kept only here.
    /// Then users can remove a resource only by the name
    /// because there's no one having the Rc except this map.
    geo_map: AHashMap<Rc<str>, GenIndexRc>,
    /// Material name to its index.
    mat_map: AHashMap<Rc<str>, GenIndexRc>,
    /// Mesh name to geometry and material indices.
    /// This map doesn't own the resources so that it works like weak references.
    /// And indices will be used without matching generations.
    meshes: AHashMap<Rc<str>, (GenIndex, GenIndex)>,
}

impl MeshResource {
    pub fn new() -> Self {
        Self {
            geos: GenVecRc::new(),
            mats: GenVecRc::new(),
            geo_map: AHashMap::new(),
            mat_map: AHashMap::new(),
            meshes: AHashMap::new(),
        }
    }

    #[inline]
    pub fn contains_geometry(&self, name: &str) -> bool {
        self.geo_map.contains_key(name)
    }

    #[inline]
    pub fn contains_material(&self, name: &str) -> bool {
        self.mat_map.contains_key(name)
    }

    #[inline]
    pub fn iter_geo(&self) -> impl Iterator<Item = &GeometryVar> {
        self.geos.iter_occupied()
    }

    #[inline]
    pub fn iter_mat(&self) -> impl Iterator<Item = &Material> {
        self.mats.iter_occupied()
    }

    /// Adds a new geometry.
    /// Returns the index of instered position if `name` is not given.
    ///
    /// # Panics
    ///
    /// Panics if `name` is given and conflicts with something.
    pub fn add_geometry(
        &mut self,
        name: Option<&Rc<str>>,
        geo: impl Into<GeometryVar>,
    ) -> Option<GenIndexRc> {
        let geo = geo.into();
        if let Some(name) = name {
            let index = self.geos.insert(geo);
            assert!(self.geo_map.insert(Rc::clone(name), index).is_none());
            None
        } else {
            Some(self.geos.insert(geo))
        }
    }

    pub fn get_geometry<'a, S: Borrow<str> + 'a>(
        &self,
        key: impl Into<LabelOrGenIndexRc<'a, S>>,
    ) -> Option<&GeometryVar> {
        match key.into() {
            LabelOrGenIndexRc::A(label) => {
                if let Some(index) = self.get_geometry_index(label.borrow()) {
                    self.geos.get(index.index)
                } else {
                    None
                }
            }
            LabelOrGenIndexRc::B(index) => self.geos.get(index.index),
        }
    }

    pub fn sneak_get_geometry_mut<'a, S: Borrow<str> + 'a>(
        &mut self,
        key: impl Into<LabelOrGenIndexRc<'a, S>>,
    ) -> Option<&mut GeometryVar> {
        match key.into() {
            LabelOrGenIndexRc::A(label) => {
                if let Some(index) = self.get_geometry_index(label.borrow()) {
                    self.geos.sneak_get_mut(index.index)
                } else {
                    None
                }
            }
            LabelOrGenIndexRc::B(index) => self.geos.sneak_get_mut(index.index),
        }
    }

    pub fn update_geometry<'a, S: Borrow<str> + 'a, U>(
        &mut self,
        key: impl Into<LabelOrGenIndexRc<'a, S>>,
        f: impl FnOnce(&mut GeometryVar) -> U,
    ) -> Option<AorB<U, (GenIndexRc, U)>> {
        match key.into() {
            LabelOrGenIndexRc::A(label) => {
                if let Some(index) = self.get_geometry_index(label.borrow()) {
                    if let Some((new_index, u)) = self.geos.update(index.index, f) {
                        // Safety: geo_map has the label.
                        unsafe {
                            *self.geo_map.get_mut(label.borrow()).unwrap_unchecked() = new_index;
                        }
                        return Some(AorB::A(u));
                    }
                }
                None
            }
            LabelOrGenIndexRc::B(index) => self.geos.update(index.index, f).map(|res| AorB::B(res)),
        }
    }

    pub fn remove_geometry(&mut self, label: &str) -> Option<GeometryVar> {
        // Safety: A mapped index must be valid and points to the occupied item.
        self.geo_map
            .remove(label)
            .map(|index| unsafe { self.geos.take(index.index).unwrap_unchecked() })
    }

    /// Adds a new material.
    /// Returns the index of inserted position if `label` is not given.
    ///
    /// # Panics
    ///
    /// Panics if `name` is given and conflicts with something.
    pub fn add_material(
        &mut self,
        label: Option<&Rc<str>>,
        mat: impl Into<Material>,
    ) -> Option<GenIndexRc> {
        let mat = mat.into();
        if let Some(label) = label {
            let index = self.mats.insert(mat);
            assert!(self.mat_map.insert(Rc::clone(label), index).is_none());
            None
        } else {
            Some(self.mats.insert(mat))
        }
    }

    pub fn get_material<'a, S: Borrow<str> + 'a>(
        &self,
        key: impl Into<LabelOrGenIndexRc<'a, S>>,
    ) -> Option<&Material> {
        match key.into() {
            LabelOrGenIndexRc::A(label) => {
                if let Some(index) = self.get_material_index(label.borrow()) {
                    self.mats.get(index.index)
                } else {
                    None
                }
            }
            LabelOrGenIndexRc::B(index) => self.mats.get(index.index),
        }
    }

    pub fn sneak_get_material_mut<'a, S: Borrow<str> + 'a>(
        &mut self,
        key: impl Into<LabelOrGenIndexRc<'a, S>>,
    ) -> Option<&mut Material> {
        match key.into() {
            LabelOrGenIndexRc::A(label) => {
                if let Some(index) = self.get_material_index(label.borrow()) {
                    self.mats.sneak_get_mut(index.index)
                } else {
                    None
                }
            }
            LabelOrGenIndexRc::B(index) => self.mats.sneak_get_mut(index.index),
        }
    }

    pub fn update_material<'a, S: Borrow<str> + 'a, U>(
        &mut self,
        key: impl Into<LabelOrGenIndexRc<'a, S>>,
        f: impl FnOnce(&mut Material) -> U,
    ) -> Option<AorB<U, (GenIndexRc, U)>> {
        match key.into() {
            LabelOrGenIndexRc::A(label) => {
                if let Some(index) = self.get_material_index(label.borrow()) {
                    if let Some((new_index, u)) = self.mats.update(index.index, f) {
                        // Safety: mat_map has the name.
                        unsafe {
                            *self.mat_map.get_mut(label.borrow()).unwrap_unchecked() = new_index;
                        }
                        return Some(AorB::A(u));
                    }
                }
                None
            }
            LabelOrGenIndexRc::B(index) => self.mats.update(index.index, f).map(|res| AorB::B(res)),
        }
    }

    pub fn remove_material(&mut self, label: &str) -> Option<Material> {
        // Safety: A mapped index must be valid and points to the occupied item.
        self.mat_map
            .remove(label)
            .map(|index| unsafe { self.mats.take(index.index).unwrap_unchecked() })
    }

    #[inline]
    pub fn add_mesh<'a, 'b, S1: Borrow<str> + 'a, S2: Borrow<str> + 'b>(
        &mut self,
        mesh_label: &Rc<str>,
        geo_key: impl Into<LabelOrGenIndexRc<'a, S1>>,
        mat_key: impl Into<LabelOrGenIndexRc<'b, S2>>,
    ) -> bool {
        self._add_mesh(mesh_label, geo_key.into(), mat_key.into())
    }

    pub fn get_mesh_geometry(&self, label: &str) -> Option<&GeometryVar> {
        if let Some((geo_index, _)) = self.meshes.get(label) {
            self.geos.get(*geo_index)
        } else {
            None
        }
    }

    pub fn get_mesh_material(&self, label: &str) -> Option<&Material> {
        if let Some((_, mat_index)) = self.meshes.get(label) {
            self.mats.get(*mat_index)
        } else {
            None
        }
    }

    fn _add_mesh<'a, S1: Borrow<str>, S2: Borrow<str>>(
        &mut self,
        mesh_label: &Rc<str>,
        geo_key: LabelOrGenIndexRc<'a, S1>,
        mat_key: LabelOrGenIndexRc<'a, S2>,
    ) -> bool {
        let geo_index = match geo_key {
            LabelOrGenIndexRc::A(label) => self.get_geometry_index(label.borrow()),
            LabelOrGenIndexRc::B(index) => Some(index),
        };
        let mat_index = match mat_key {
            LabelOrGenIndexRc::A(label) => self.get_material_index(label.borrow()),
            LabelOrGenIndexRc::B(index) => Some(index),
        };
        if let (Some(geo_index), Some(mat_index)) = (geo_index, mat_index) {
            // Put in forced `GenIndex`.
            self.meshes.insert(
                Rc::clone(mesh_label),
                (geo_index.index.into_forced(), mat_index.index.into_forced()),
            );
            true
        } else {
            false
        }
    }

    /// Users can't acquire `GenIndexRc` from the its name.
    /// Because [`Self::geo_map`] and [`Self::mat_map`] are the only one which have Rc.
    /// It's important with respect to remove operations.
    #[inline]
    fn get_geometry_index(&self, label: &str) -> Option<&GenIndexRc> {
        self.geo_map.get(label)
    }

    /// Users can't acquire `GenIndexRc` from the its name.
    /// Because [`Self::geo_map`] and [`Self::mat_map`] are the only one which have Rc.
    /// It's important with respect to remove operations.
    #[inline]
    fn get_material_index(&self, label: &str) -> Option<&GenIndexRc> {
        self.mat_map.get(label)
    }
}

impl Default for MeshResource {
    fn default() -> Self {
        Self::new()
    }
}

/// Mesh.primitive.attributes(Geometry) in glTF.
/// See https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#meshes
#[derive(Debug, Clone, Default)]
pub struct Geometry {
    // 16 is the default limits on WebGPU and WebGL.
    /// Geometry attributes such as position vector and normal vector.
    attrs: SmallVec<[GeometryAttribute; 16]>,
    indices: GeometryIndices,
    position_index: usize,
    normal_index: usize,
    tangent_index: usize,
    uv_index: usize,
    color_index: usize,
    joint_index: usize,
    weight_index: usize,
    user_indices: [usize; 4], // From A to D
}

impl Geometry {
    const INVALID_INDEX: usize = usize::MAX;

    pub fn new() -> Self {
        Self {
            attrs: SmallVec::new(),
            indices: GeometryIndices::new(),
            position_index: Self::INVALID_INDEX,
            normal_index: Self::INVALID_INDEX,
            tangent_index: Self::INVALID_INDEX,
            uv_index: Self::INVALID_INDEX,
            color_index: Self::INVALID_INDEX,
            joint_index: Self::INVALID_INDEX,
            weight_index: Self::INVALID_INDEX,
            user_indices: [Self::INVALID_INDEX; 4],
        }
    }

    #[inline]
    #[must_use]
    pub fn with_position(mut self, values: GeometryAttributeValues) -> Self {
        self.position_index =
            self.put_attribute(self.position_index, GeometryAttribute::Position(values));
        self
    }

    #[inline]
    #[must_use]
    pub fn with_normal(mut self, values: GeometryAttributeValues) -> Self {
        self.normal_index =
            self.put_attribute(self.normal_index, GeometryAttribute::Normal(values));
        self
    }

    #[inline]
    #[must_use]
    pub fn with_tangent(mut self, values: GeometryAttributeValues) -> Self {
        self.tangent_index =
            self.put_attribute(self.tangent_index, GeometryAttribute::Tangent(values));
        self
    }

    #[must_use]
    pub fn with_uv(mut self, values: GeometryAttributeValues) -> Self {
        if self.uv_index == Self::INVALID_INDEX {
            self.uv_index =
                self.put_attribute(self.uv_index, GeometryAttribute::TexCoord(vec![values]));
        } else {
            assert_eq!(self.attrs[0].len(), values.len());
            self.attrs[self.uv_index].insert(values);
        }
        self
    }

    #[must_use]
    pub fn with_color(mut self, values: GeometryAttributeValues) -> Self {
        if self.color_index == Self::INVALID_INDEX {
            self.color_index =
                self.put_attribute(self.color_index, GeometryAttribute::Color(vec![values]));
        } else {
            assert_eq!(self.attrs[0].len(), values.len());
            self.attrs[self.color_index].insert(values);
        }
        self
    }

    #[must_use]
    pub fn with_user(mut self, user: char, values: GeometryAttributeValues) -> Self {
        assert!(matches!(user, 'A'..='D' | 'a'..='d'));
        let ui = user.to_ascii_uppercase() as usize - 'A' as usize;
        if self.user_indices[ui] == Self::INVALID_INDEX {
            let attr = match ui {
                0 => GeometryAttribute::UserA(vec![values]),
                1 => GeometryAttribute::UserB(vec![values]),
                2 => GeometryAttribute::UserC(vec![values]),
                3 => GeometryAttribute::UserD(vec![values]),
                _ => unreachable!(),
            };
            self.user_indices[ui] = self.put_attribute(self.user_indices[ui], attr);
        } else {
            assert_eq!(self.attrs[0].len(), values.len());
            self.attrs[self.user_indices[ui]].insert(values);
        }
        self
    }

    pub fn get_attribute(&self, i: usize) -> Option<&GeometryAttribute> {
        self.attrs.get(i)
    }

    pub fn get_attribute_mut(&mut self, i: usize) -> Option<&mut GeometryAttribute> {
        self.attrs.get_mut(i)
    }

    #[inline]
    pub fn get_position(&self) -> Option<&GeometryAttributeValues> {
        self.attrs
            .get(self.position_index)
            .map(|attr| attr.as_values(0))
    }

    #[inline]
    pub fn get_position_mut(&mut self) -> Option<&mut GeometryAttributeValues> {
        self.attrs
            .get_mut(self.position_index)
            .map(|attr| attr.as_values_mut(0))
    }

    #[inline]
    pub fn get_normal(&self) -> Option<&GeometryAttributeValues> {
        self.attrs
            .get(self.normal_index)
            .map(|attr| attr.as_values(0))
    }

    #[inline]
    pub fn get_normal_mut(&mut self) -> Option<&mut GeometryAttributeValues> {
        self.attrs
            .get_mut(self.normal_index)
            .map(|attr| attr.as_values_mut(0))
    }

    #[inline]
    pub fn get_tangent(&self) -> Option<&GeometryAttributeValues> {
        self.attrs
            .get(self.tangent_index)
            .map(|attr| attr.as_values(0))
    }

    #[inline]
    pub fn get_tangent_mut(&mut self) -> Option<&mut GeometryAttributeValues> {
        self.attrs
            .get_mut(self.tangent_index)
            .map(|attr| attr.as_values_mut(0))
    }

    #[inline]
    pub fn get_uv(&self, i: usize) -> Option<&GeometryAttributeValues> {
        self.attrs.get(self.uv_index).map(|attr| attr.as_values(i))
    }

    #[inline]
    pub fn get_uv_mut(&mut self, i: usize) -> Option<&mut GeometryAttributeValues> {
        self.attrs
            .get_mut(self.uv_index)
            .map(|attr| attr.as_values_mut(i))
    }

    #[inline]
    pub fn get_color(&self, i: usize) -> Option<&GeometryAttributeValues> {
        self.attrs
            .get(self.color_index)
            .map(|attr| attr.as_values(i))
    }

    #[inline]
    pub fn get_color_mut(&mut self, i: usize) -> Option<&mut GeometryAttributeValues> {
        self.attrs
            .get_mut(self.color_index)
            .map(|attr| attr.as_values_mut(i))
    }

    #[inline]
    #[must_use]
    pub fn with_indices(mut self, indices: GeometryIndices) -> Self {
        self.indices = indices;
        self
    }

    #[inline]
    pub fn get_indices(&self) -> &GeometryIndices {
        &self.indices
    }

    /// If `index` is valid, `attr` is inserted into the position.
    /// Otherwise, `attr` is appended.
    ///
    /// Returns the inserted index.
    ///
    /// # Panics
    ///
    /// Panics if `attr` has different length.
    fn put_attribute(&mut self, index: usize, attr: GeometryAttribute) -> usize {
        // All attributes must have the same length.
        if !self.attrs.is_empty() {
            assert_eq!(self.attrs[0].len(), attr.len());
        }
        if index < self.attrs.len() {
            self.attrs[index] = attr;
            index
        } else {
            self.attrs.push(attr);
            self.attrs.len() - 1
        }
    }

    /// Sorts attributes by the default order looks like something below.
    ///
    /// POSITION, NORMAL, TANGENT,
    /// TEXCOORD_0(uv), TEXCOORD_1, ...
    /// Color_0, Color_1, ...
    fn sort(&mut self) {
        self.attrs.sort_unstable_by_key(|attr| attr.order());
        for (i, attr) in self.attrs.iter().enumerate() {
            match attr {
                GeometryAttribute::Position(..) => self.position_index = i,
                GeometryAttribute::Normal(..) => self.normal_index = i,
                GeometryAttribute::Tangent(..) => self.tangent_index = i,
                GeometryAttribute::TexCoord(..) => self.uv_index = i,
                GeometryAttribute::Color(..) => self.color_index = i,
                GeometryAttribute::Joint(..) => self.joint_index = i,
                GeometryAttribute::Weight(..) => self.weight_index = i,
                GeometryAttribute::UserA(..) => self.user_indices[0] = i,
                GeometryAttribute::UserB(..) => self.user_indices[1] = i,
                GeometryAttribute::UserC(..) => self.user_indices[2] = i,
                GeometryAttribute::UserD(..) => self.user_indices[3] = i,
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum GeometryAttributeVariant {
    Position,
    Normal,
    Tangent,
    TexCoord,
    Color,
    Joint,
    Weight,
    UserA,
    UserB,
    UserC,
    UserD,
}

impl From<&GeometryAttribute> for GeometryAttributeVariant {
    fn from(value: &GeometryAttribute) -> Self {
        match value {
            GeometryAttribute::Position(..) => Self::Position,
            GeometryAttribute::Normal(..) => Self::Normal,
            GeometryAttribute::Tangent(..) => Self::Tangent,
            GeometryAttribute::TexCoord(..) => Self::TexCoord,
            GeometryAttribute::Color(..) => Self::Color,
            GeometryAttribute::Joint(..) => Self::Joint,
            GeometryAttribute::Weight(..) => Self::Weight,
            GeometryAttribute::UserA(..) => Self::UserA,
            GeometryAttribute::UserB(..) => Self::UserB,
            GeometryAttribute::UserC(..) => Self::UserC,
            GeometryAttribute::UserD(..) => Self::UserD,
        }
    }
}

#[derive(Debug, Clone)]
pub enum GeometryAttribute {
    Position(GeometryAttributeValues),
    Normal(GeometryAttributeValues),
    Tangent(GeometryAttributeValues),
    TexCoord(Vec<GeometryAttributeValues>), // TEXTCOORD_n, They must have the same length.
    Color(Vec<GeometryAttributeValues>),    // COLOR_n, They must have the same length.
    Joint(Vec<GeometryAttributeValues>),    // JOINTS_n, They must have the same length.
    Weight(Vec<GeometryAttributeValues>),   // WEIGHTS_n, They must have the same length.
    UserA(Vec<GeometryAttributeValues>),
    UserB(Vec<GeometryAttributeValues>),
    UserC(Vec<GeometryAttributeValues>),
    UserD(Vec<GeometryAttributeValues>),
}

impl GeometryAttribute {
    #[inline]
    pub fn num(&self) -> usize {
        match self {
            Self::Position(..) => 1,
            Self::Normal(..) => 1,
            Self::Tangent(..) => 1,
            Self::TexCoord(vv) => vv.len(),
            Self::Color(vv) => vv.len(),
            Self::Joint(vv) => vv.len(),
            Self::Weight(vv) => vv.len(),
            Self::UserA(vv) => vv.len(),
            Self::UserB(vv) => vv.len(),
            Self::UserC(vv) => vv.len(),
            Self::UserD(vv) => vv.len(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        match self {
            Self::Position(v) => v.len(),
            Self::Normal(v) => v.len(),
            Self::Tangent(v) => v.len(),
            Self::TexCoord(vv) => vv[0].len(),
            Self::Color(vv) => vv[0].len(),
            Self::Joint(vv) => vv[0].len(),
            Self::Weight(vv) => vv[0].len(),
            Self::UserA(vv) => vv[0].len(),
            Self::UserB(vv) => vv[0].len(),
            Self::UserC(vv) => vv[0].len(),
            Self::UserD(vv) => vv[0].len(),
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn unit_size(&self, inner_i: usize) -> usize {
        match self {
            Self::Position(v) => v.unit_size(),
            Self::Normal(v) => v.unit_size(),
            Self::Tangent(v) => v.unit_size(),
            Self::TexCoord(vv) => vv[inner_i].unit_size(),
            Self::Color(vv) => vv[inner_i].unit_size(),
            Self::Joint(vv) => vv[inner_i].unit_size(),
            Self::Weight(vv) => vv[inner_i].unit_size(),
            Self::UserA(vv) => vv[inner_i].unit_size(),
            Self::UserB(vv) => vv[inner_i].unit_size(),
            Self::UserC(vv) => vv[inner_i].unit_size(),
            Self::UserD(vv) => vv[inner_i].unit_size(),
        }
    }

    #[inline]
    pub fn as_values(&self, inner_i: usize) -> &GeometryAttributeValues {
        match self {
            Self::Position(v) => v,
            Self::Normal(v) => v,
            Self::Tangent(v) => v,
            Self::TexCoord(vv) => &vv[inner_i],
            Self::Color(vv) => &vv[inner_i],
            Self::Joint(vv) => &vv[inner_i],
            Self::Weight(vv) => &vv[inner_i],
            Self::UserA(vv) => &vv[inner_i],
            Self::UserB(vv) => &vv[inner_i],
            Self::UserC(vv) => &vv[inner_i],
            Self::UserD(vv) => &vv[inner_i],
        }
    }

    #[inline]
    pub fn as_values_mut(&mut self, inner_i: usize) -> &mut GeometryAttributeValues {
        match self {
            Self::Position(v) => v,
            Self::Normal(v) => v,
            Self::Tangent(v) => v,
            Self::TexCoord(vv) => &mut vv[inner_i],
            Self::Color(vv) => &mut vv[inner_i],
            Self::Joint(vv) => &mut vv[inner_i],
            Self::Weight(vv) => &mut vv[inner_i],
            Self::UserA(vv) => &mut vv[inner_i],
            Self::UserB(vv) => &mut vv[inner_i],
            Self::UserC(vv) => &mut vv[inner_i],
            Self::UserD(vv) => &mut vv[inner_i],
        }
    }

    #[inline]
    pub fn format(&self, inner_i: usize) -> wgpu::VertexFormat {
        match self {
            Self::Position(v) => v.format(),
            Self::Normal(v) => v.format(),
            Self::Tangent(v) => v.format(),
            Self::TexCoord(vv) => vv[inner_i].format(),
            Self::Color(vv) => vv[inner_i].format(),
            Self::Joint(vv) => vv[inner_i].format(),
            Self::Weight(vv) => vv[inner_i].format(),
            Self::UserA(vv) => vv[inner_i].format(),
            Self::UserB(vv) => vv[inner_i].format(),
            Self::UserC(vv) => vv[inner_i].format(),
            Self::UserD(vv) => vv[inner_i].format(),
        }
    }

    /// If this attribute is a vector of attribute values,
    /// the attribute is replaced with the `values`.
    ///
    /// If this is a nested vector of attribute values,
    /// `values` is appended to the end of nested vector.
    #[inline]
    pub fn insert(&mut self, values: GeometryAttributeValues) {
        match self {
            Self::Position(v) => *v = values,
            Self::Normal(v) => *v = values,
            Self::Tangent(v) => *v = values,
            Self::TexCoord(vv) => vv.push(values),
            Self::Color(vv) => vv.push(values),
            Self::Joint(vv) => vv.push(values),
            Self::Weight(vv) => vv.push(values),
            Self::UserA(vv) => vv.push(values),
            Self::UserB(vv) => vv.push(values),
            Self::UserC(vv) => vv.push(values),
            Self::UserD(vv) => vv.push(values),
        }
    }

    #[inline]
    fn order(&self) -> usize {
        match self {
            Self::Position(..) => 0,
            Self::Normal(..) => 1,
            Self::Tangent(..) => 2,
            Self::TexCoord(..) => 3,
            Self::Color(..) => 4,
            Self::Joint(..) => 5,
            Self::Weight(..) => 6,
            Self::UserA(..) => 7,
            Self::UserB(..) => 8,
            Self::UserC(..) => 9,
            Self::UserD(..) => 10,
        }
    }
}

#[derive(Debug, Clone)]
pub enum GeometryAttributeValues {
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

impl GeometryAttributeValues {
    #[inline]
    pub fn from_vec_uint8x4(v: Vec<Vector<u8, 4>>) -> Self {
        Self::VecUint8x4(v)
    }

    #[inline]
    pub fn as_vec_uint8x4(&self) -> &Vec<Vector<u8, 4>> {
        match self {
            Self::VecUint8x4(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn as_vec_mut_uint8x4(&mut self) -> &mut Vec<Vector<u8, 4>> {
        match self {
            Self::VecUint8x4(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn from_vec_uint16x4(v: Vec<Vector<u16, 4>>) -> Self {
        Self::VecUint16x4(v)
    }

    #[inline]
    pub fn as_vec_uint16x4(&self) -> &Vec<Vector<u16, 4>> {
        match self {
            Self::VecUint16x4(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn as_vec_mut_uint16x4(&mut self) -> &mut Vec<Vector<u16, 4>> {
        match self {
            Self::VecUint16x4(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
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

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn unit_size(&self) -> usize {
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

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
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

    #[inline]
    pub fn format(&self) -> wgpu::VertexFormat {
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

macro_rules! impl_from_for_geometry_attribute_values {
    ($x:ty, $y:ident) => {
        impl From<Vec<$x>> for GeometryAttributeValues {
            fn from(value: Vec<$x>) -> Self {
                Self::$y(value)
            }
        }

        impl<'a> From<&'a GeometryAttributeValues> for &'a Vec<$x> {
            fn from(value: &'a GeometryAttributeValues) -> Self {
                match value {
                    GeometryAttributeValues::$y(v) => v,
                    _ => panic!(),
                }
            }
        }

        impl<'a> From<&'a mut GeometryAttributeValues> for &'a mut Vec<$x> {
            fn from(value: &'a mut GeometryAttributeValues) -> Self {
                match value {
                    GeometryAttributeValues::$y(v) => v,
                    _ => panic!(),
                }
            }
        }
    };
}

// Use these instead of From::from()
// GeometryAttributeValues::from_vec_uint8x4()
// GeometryAttributeValues::as_vec_uint8x4()
// GeometryAttributeValues::as_vec_mut_uint8x4()
// impl_from_for_geometry_attribute_values!(Vector<u8, 4>, VecUint8x4);
impl_from_for_geometry_attribute_values!(Vector<u8, 2>, VecUnorm8x2);
impl_from_for_geometry_attribute_values!(Vector<u8, 4>, VecUnorm8x4);
// Use these instead of From::from()
// GeometryAttributeValues::from_vec_uint16x4()
// GeometryAttributeValues::as_vec_uint16x4()
// GeometryAttributeValues::as_vec_mut_uint16x4()
// impl_from_for_geometry_attribute_values!(Vector<u16, 4>, VecUint16x4);
impl_from_for_geometry_attribute_values!(Vector<u16, 2>, VecUnorm16x2);
impl_from_for_geometry_attribute_values!(Vector<u16, 4>, VecUnorm16x4);
impl_from_for_geometry_attribute_values!(Vector<f32, 2>, VecFloat32x2);
impl_from_for_geometry_attribute_values!(Vector<f32, 3>, VecFloat32x3);
impl_from_for_geometry_attribute_values!(Vector<f32, 4>, VecFloat32x4);

// TODO: With buffer, modification functions, and other utilities.
#[derive(Debug, Default, Clone)]
pub struct InterleavedGeometry {
    pub vertex_bytes: Vec<u8>,
    pub attrs: Vec<wgpu::VertexAttribute>,
    pub attr_kinds: Vec<GeometryAttributeVariant>,
    pub vertex_size: usize,
    pub vertex_num: usize,
    pub index_bytes: Vec<u8>,
    pub index_format: wgpu::IndexFormat,
    pub index_num: usize,
}

impl From<&Geometry> for InterleavedGeometry {
    /// It's recommended to sort before generating `InterleavedGeometry`.
    // No padding.
    fn from(value: &Geometry) -> Self {
        // Prepares buffers.
        let vertex_size = value.attrs.iter().fold(0, |acc, attr| {
            acc + (0..attr.num())
                .map(|inner_i| attr.unit_size(inner_i))
                .sum::<usize>()
        });
        let vertex_num = value
            .attrs
            .first()
            .map(|attr| attr.len())
            .unwrap_or_default();
        let mut vertex_bytes = vec![0_u8; vertex_size * vertex_num];
        let mut attrs = Vec::with_capacity(value.attrs.len());
        let mut attr_kinds = Vec::with_capacity(value.attrs.len());

        let mut attr_offset = 0;
        let mut shader_location = 0;
        for attr in value.attrs.iter() {
            for inner_i in 0..attr.num() {
                let chunk_size = attr.unit_size(inner_i);
                let chunks = attr.as_values(inner_i).as_bytes();
                let chunk_iter = chunks.chunks_exact(chunk_size);
                for (vertex_idx, chunk) in chunk_iter.enumerate() {
                    let s = vertex_idx * vertex_size + attr_offset;
                    vertex_bytes[s..(s + chunk_size)].copy_from_slice(chunk);
                }

                attrs.push(wgpu::VertexAttribute {
                    format: attr.format(inner_i),
                    offset: attr_offset as wgpu::BufferAddress,
                    shader_location,
                });

                attr_kinds.push(GeometryAttributeVariant::from(attr));

                attr_offset += chunk_size;
                shader_location += 1;
            }
        }

        InterleavedGeometry {
            vertex_bytes,
            attrs,
            attr_kinds,
            vertex_size,
            vertex_num,
            index_bytes: value.indices.clone().into(),
            index_format: value.indices.format(),
            index_num: value.indices.len(),
        }
    }
}

impl From<&mut Geometry> for InterleavedGeometry {
    #[inline]
    fn from(value: &mut Geometry) -> Self {
        InterleavedGeometry::from(value as &Geometry)
    }
}

impl AsMultiBytes for InterleavedGeometry {
    fn as_bytes(&self, index: usize) -> &[u8] {
        match index {
            0 => &self.vertex_bytes,
            1 => &self.index_bytes,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum GeometryIndices {
    U16(Vec<u16>),
    U32(Vec<u32>),
}

impl GeometryIndices {
    pub const fn new() -> GeometryIndices {
        Self::U16(vec![])
    }

    #[inline]
    pub fn len(&self) -> usize {
        match self {
            Self::U16(v) => v.len(),
            Self::U32(v) => v.len(),
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn format(&self) -> wgpu::IndexFormat {
        match self {
            Self::U16(..) => wgpu::IndexFormat::Uint16,
            Self::U32(..) => wgpu::IndexFormat::Uint32,
        }
    }
}

impl Default for GeometryIndices {
    fn default() -> Self {
        Self::U16(Vec::new())
    }
}

impl From<Vec<u16>> for GeometryIndices {
    fn from(value: Vec<u16>) -> Self {
        GeometryIndices::U16(value)
    }
}

impl From<Vec<u32>> for GeometryIndices {
    fn from(value: Vec<u32>) -> Self {
        GeometryIndices::U32(value)
    }
}

impl From<Vec<i32>> for GeometryIndices {
    fn from(value: Vec<i32>) -> Self {
        GeometryIndices::U32(bytemuck::cast_vec(value))
    }
}

impl From<GeometryIndices> for Vec<u8> {
    fn from(value: GeometryIndices) -> Self {
        match value {
            GeometryIndices::U16(v) => {
                let casted: &[u8] = bytemuck::cast_slice(&v[..]);
                Vec::from(casted)
            }
            GeometryIndices::U32(v) => {
                let casted: &[u8] = bytemuck::cast_slice(&v[..]);
                Vec::from(casted)
            }
        }
    }
}

impl<'a> From<&'a GeometryIndices> for &'a [u8] {
    fn from(value: &'a GeometryIndices) -> Self {
        match value {
            GeometryIndices::U16(v) => bytemuck::cast_slice(v),
            GeometryIndices::U32(v) => bytemuck::cast_slice(v),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Material {
    /// Default.
    Simple(SimpleMaterial),
}

impl Default for Material {
    fn default() -> Self {
        Self::Simple(SimpleMaterial::default())
    }
}

impl<'a> From<&'a Material> for &'a [u8] {
    fn from(value: &'a Material) -> Self {
        match value {
            Material::Simple(mat) => mat.into(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
pub struct SimpleMaterial {
    /// Color of the material.
    /// Range: [0, 255]
    /// Default: [0, 0, 0]
    color: Color,
}

impl<'a> From<&'a SimpleMaterial> for &'a [u8] {
    fn from(value: &'a SimpleMaterial) -> Self {
        bytemuck::cast_slice(std::slice::from_ref(value))
    }
}

impl Default for SimpleMaterial {
    fn default() -> Self {
        Self {
            color: colors::BLACK,
        }
    }
}

impl From<SimpleMaterial> for Material {
    fn from(value: SimpleMaterial) -> Self {
        Self::Simple(value)
    }
}

impl From<Color> for SimpleMaterial {
    fn from(value: Color) -> Self {
        Self { color: value }
    }
}

impl From<Color> for Material {
    fn from(value: Color) -> Self {
        Material::from(SimpleMaterial::from(value))
    }
}
