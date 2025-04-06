use super::common::{HasLabel, LabelledGenVec};
use crate::{
    ActteyError,
    util::{AsOr, StaticStr},
};
use my_ecs::prelude::{Or, Resource, ResourceId, ResourceIndex};
use std::{fmt, ops::Deref, sync::Arc};

#[derive(Debug, Resource)]
pub struct BindGroupStorage {
    dev: Arc<wgpu::Device>,
    ri: ResourceIndex,
    groups: LabelledGenVec<BindGroup>,
}

impl BindGroupStorage {
    pub(crate) fn new(dev: Arc<wgpu::Device>) -> Self {
        Self {
            dev,
            ri: ResourceIndex::dummy(),
            groups: LabelledGenVec::new(),
        }
    }

    pub(crate) fn set_resource_index(&mut self, ri: ResourceIndex) {
        self.ri = ri;
    }

    pub fn create_then_add(
        &mut self,
        desc: &wgpu::BindGroupDescriptor<'_>,
    ) -> Result<ResourceId, ActteyError> {
        let group = self.create(desc);
        self.add(group)
    }

    pub fn create(&self, desc: &wgpu::BindGroupDescriptor<'_>) -> BindGroup {
        let label = desc
            .label
            .map(|s| StaticStr::new(s.to_owned()))
            .unwrap_or_default();
        let group = self.dev.create_bind_group(desc);
        BindGroup::new(label, group)
    }

    pub fn add(&mut self, group: BindGroup) -> Result<ResourceId, ActteyError> {
        debug_assert!(!self.ri.is_dummy());

        let ii = self.groups.add(group)?;
        let rid = ResourceId::new(self.ri, ii);
        Ok(rid)
    }

    pub fn remove<K>(&mut self, key: K) -> Option<BindGroup>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner(this: &mut BindGroupStorage, key: Or<&ResourceId, &str>) -> Option<BindGroup> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.groups.remove(key)
        }
    }

    pub fn get<K>(&self, key: K) -> Option<&BindGroup>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner<'s>(
            this: &'s BindGroupStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<&'s BindGroup> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.groups.get(key)
        }
    }
}

#[derive(Debug, Resource)]
pub struct BindGroupLayoutStorage {
    dev: Arc<wgpu::Device>,
    ri: ResourceIndex,
    layouts: LabelledGenVec<BindGroupLayout>,
}

impl BindGroupLayoutStorage {
    pub(crate) fn new(dev: Arc<wgpu::Device>) -> Self {
        Self {
            dev,
            ri: ResourceIndex::dummy(),
            layouts: LabelledGenVec::new(),
        }
    }

    pub(crate) fn set_resource_index(&mut self, ri: ResourceIndex) {
        self.ri = ri;
    }

    pub fn create_then_add(
        &mut self,
        desc: &wgpu::BindGroupLayoutDescriptor<'_>,
    ) -> Result<ResourceId, ActteyError> {
        let layout = self.create(desc);
        self.add(layout)
    }

    pub fn create(&self, desc: &wgpu::BindGroupLayoutDescriptor<'_>) -> BindGroupLayout {
        let label = desc
            .label
            .map(|s| StaticStr::new(s.to_owned()))
            .unwrap_or_default();
        let layout = self.dev.create_bind_group_layout(desc);
        BindGroupLayout::new(label, layout)
    }

    pub fn add(&mut self, layout: BindGroupLayout) -> Result<ResourceId, ActteyError> {
        debug_assert!(!self.ri.is_dummy());

        let ii = self.layouts.add(layout)?;
        let rid = ResourceId::new(self.ri, ii);
        Ok(rid)
    }

    pub fn remove<K>(&mut self, key: K) -> Option<BindGroupLayout>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner(
            this: &mut BindGroupLayoutStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<BindGroupLayout> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.layouts.remove(key)
        }
    }

    pub fn get<K>(&self, key: K) -> Option<&BindGroupLayout>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner<'s>(
            this: &'s BindGroupLayoutStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<&'s BindGroupLayout> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.layouts.get(key)
        }
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct BindGroup(Arc<BindGroupInner>);

impl BindGroup {
    fn new(label: StaticStr, group: wgpu::BindGroup) -> Self {
        let inner = Arc::new(BindGroupInner { label, group });
        Self(inner)
    }
}

impl HasLabel for BindGroup {
    fn label(&self) -> &str {
        &self.0.label
    }
}

impl Deref for BindGroup {
    type Target = wgpu::BindGroup;

    fn deref(&self) -> &Self::Target {
        &self.0.group
    }
}

impl fmt::Debug for BindGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = &*self.0;
        inner.fmt(f)
    }
}

#[derive(Debug)]
struct BindGroupInner {
    label: StaticStr,
    group: wgpu::BindGroup,
}

#[derive(Clone)]
#[repr(transparent)]
pub struct BindGroupLayout(Arc<BindGroupLayoutInner>);

impl BindGroupLayout {
    fn new(label: StaticStr, layout: wgpu::BindGroupLayout) -> Self {
        let inner = Arc::new(BindGroupLayoutInner { label, layout });
        Self(inner)
    }
}

impl HasLabel for BindGroupLayout {
    fn label(&self) -> &str {
        &self.0.label
    }
}

impl Deref for BindGroupLayout {
    type Target = wgpu::BindGroupLayout;

    fn deref(&self) -> &Self::Target {
        &self.0.layout
    }
}

impl fmt::Debug for BindGroupLayout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = &*self.0;
        inner.fmt(f)
    }
}

#[derive(Debug)]
struct BindGroupLayoutInner {
    label: StaticStr,
    layout: wgpu::BindGroupLayout,
}
