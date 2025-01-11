use super::common::{HasLabel, LabelledGenVec};
use crate::{
    util::{AsOr, Or, StaticStr},
    ActteyError,
};
use my_ecs::prelude::{Resource, ResourceId, ResourceIndex};
use std::{fmt, ops::Deref, sync::Arc};

#[derive(Debug, Resource)]
pub struct ComputePipelineStorage {
    dev: Arc<wgpu::Device>,
    ri: ResourceIndex,
    pipes: LabelledGenVec<ComputePipeline>,
}

impl ComputePipelineStorage {
    pub(crate) fn new(dev: Arc<wgpu::Device>) -> Self {
        Self {
            dev,
            ri: ResourceIndex::dummy(),
            pipes: LabelledGenVec::new(),
        }
    }

    pub(crate) fn set_resource_index(&mut self, ri: ResourceIndex) {
        self.ri = ri;
    }

    pub fn create_then_add(
        &mut self,
        desc: &wgpu::ComputePipelineDescriptor<'_>,
    ) -> Result<ResourceId, ActteyError> {
        let pipe = self.create(desc);
        self.add(pipe)
    }

    pub fn create(&self, desc: &wgpu::ComputePipelineDescriptor<'_>) -> ComputePipeline {
        let label = desc
            .label
            .map(|s| StaticStr::new(s.to_owned()))
            .unwrap_or_default();
        let pipe = self.dev.create_compute_pipeline(desc);
        ComputePipeline::new(label, pipe)
    }

    pub fn add(&mut self, pipe: ComputePipeline) -> Result<ResourceId, ActteyError> {
        debug_assert!(!self.ri.is_dummy());

        let ii = self.pipes.add(pipe)?;
        let rid = ResourceId::new(self.ri, ii);
        Ok(rid)
    }

    pub fn remove<K>(&mut self, key: K) -> Option<ComputePipeline>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner(
            this: &mut ComputePipelineStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<ComputePipeline> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.pipes.remove(key)
        }
    }

    pub fn get<K>(&self, key: K) -> Option<&ComputePipeline>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner<'s>(
            this: &'s ComputePipelineStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<&'s ComputePipeline> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.pipes.get(key)
        }
    }
}

#[derive(Debug, Resource)]
pub struct PipelineLayoutStorage {
    dev: Arc<wgpu::Device>,
    ri: ResourceIndex,
    layouts: LabelledGenVec<PipelineLayout>,
}

impl PipelineLayoutStorage {
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
        desc: &wgpu::PipelineLayoutDescriptor,
    ) -> Result<ResourceId, ActteyError> {
        let layout = self.create(desc);
        self.add(layout)
    }

    pub fn create(&self, desc: &wgpu::PipelineLayoutDescriptor) -> PipelineLayout {
        let label = desc
            .label
            .map(|s| StaticStr::new(s.to_owned()))
            .unwrap_or_default();
        let layout = self.dev.create_pipeline_layout(desc);
        PipelineLayout::new(label, layout)
    }

    pub fn add(&mut self, layout: PipelineLayout) -> Result<ResourceId, ActteyError> {
        debug_assert!(!self.ri.is_dummy());

        let ii = self.layouts.add(layout)?;
        let rid = ResourceId::new(self.ri, ii);
        Ok(rid)
    }

    pub fn remove<K>(&mut self, key: K) -> Option<PipelineLayout>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner(
            this: &mut PipelineLayoutStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<PipelineLayout> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.layouts.remove(key)
        }
    }

    pub fn get<K>(&self, key: K) -> Option<&PipelineLayout>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner<'s>(
            this: &'s PipelineLayoutStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<&'s PipelineLayout> {
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
pub struct ComputePipeline(Arc<ComputePipelineInner>);

impl ComputePipeline {
    fn new(label: StaticStr, pipe: wgpu::ComputePipeline) -> Self {
        let inner = Arc::new(ComputePipelineInner { label, pipe });
        Self(inner)
    }
}

impl HasLabel for ComputePipeline {
    fn label(&self) -> &str {
        &self.0.label
    }
}

impl Deref for ComputePipeline {
    type Target = wgpu::ComputePipeline;

    fn deref(&self) -> &Self::Target {
        &self.0.pipe
    }
}

impl fmt::Debug for ComputePipeline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = &*self.0;
        inner.fmt(f)
    }
}

#[derive(Debug)]
struct ComputePipelineInner {
    label: StaticStr,
    pipe: wgpu::ComputePipeline,
}

#[derive(Clone)]
#[repr(transparent)]
pub struct PipelineLayout(Arc<PipelineLayoutInner>);

impl PipelineLayout {
    fn new(label: StaticStr, layout: wgpu::PipelineLayout) -> Self {
        let inner = Arc::new(PipelineLayoutInner { label, layout });
        Self(inner)
    }
}

impl HasLabel for PipelineLayout {
    fn label(&self) -> &str {
        &self.0.label
    }
}

impl Deref for PipelineLayout {
    type Target = wgpu::PipelineLayout;

    fn deref(&self) -> &Self::Target {
        &self.0.layout
    }
}

impl fmt::Debug for PipelineLayout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = &*self.0;
        inner.fmt(f)
    }
}

#[derive(Debug)]
struct PipelineLayoutInner {
    label: StaticStr,
    layout: wgpu::PipelineLayout,
}
