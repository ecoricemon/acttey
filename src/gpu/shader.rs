use super::common::{HasLabel, LabelledGenVec};
use crate::{
    util::{AsOr, StaticStr},
    ActteyError,
};
use my_ecs::prelude::{Or, Resource, ResourceId, ResourceIndex};
use std::{fmt, ops::Deref, sync::Arc};

#[derive(Debug, Resource)]
pub struct ShaderStorage {
    dev: Arc<wgpu::Device>,
    ri: ResourceIndex,
    shaders: LabelledGenVec<Shader>,
}

impl ShaderStorage {
    pub(crate) fn new(dev: Arc<wgpu::Device>) -> Self {
        Self {
            dev,
            ri: ResourceIndex::dummy(),
            shaders: LabelledGenVec::new(),
        }
    }

    pub(crate) fn set_resource_index(&mut self, ri: ResourceIndex) {
        self.ri = ri;
    }

    pub fn create_then_add(
        &mut self,
        desc: wgpu::ShaderModuleDescriptor<'_>,
        entry: EntryPoint,
    ) -> Result<ResourceId, ActteyError> {
        let shader = self.create(desc, entry);
        self.add(shader)
    }

    pub fn create(&self, desc: wgpu::ShaderModuleDescriptor<'_>, entry: EntryPoint) -> Shader {
        let label = desc
            .label
            .map(|s| StaticStr::new(s.to_owned()))
            .unwrap_or_default();
        let module = self.dev.create_shader_module(desc);
        Shader::new(label, module, entry)
    }

    pub fn add(&mut self, shader: Shader) -> Result<ResourceId, ActteyError> {
        debug_assert!(!self.ri.is_dummy());

        let ii = self.shaders.add(shader)?;
        let rid = ResourceId::new(self.ri, ii);
        Ok(rid)
    }

    pub fn remove<K>(&mut self, key: K) -> Option<Shader>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner(this: &mut ShaderStorage, key: Or<&ResourceId, &str>) -> Option<Shader> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.shaders.remove(key)
        }
    }

    pub fn get<K>(&self, key: K) -> Option<&Shader>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner<'s>(this: &'s ShaderStorage, key: Or<&ResourceId, &str>) -> Option<&'s Shader> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.shaders.get(key)
        }
    }
}

#[derive(Clone)]
pub struct Shader {
    module: Arc<ShaderModule>,
    entry: EntryPoint,
}

impl Shader {
    fn new(label: StaticStr, module: wgpu::ShaderModule, entry: EntryPoint) -> Self {
        let module = Arc::new(ShaderModule {
            label,
            inner: module,
        });
        Self { module, entry }
    }

    pub fn is_vertex(&self) -> bool {
        match &self.entry {
            EntryPoint::Vertex(..) => true,
            EntryPoint::Fragment(..) => false,
            EntryPoint::Compute(..) => false,
        }
    }

    pub fn is_fragment(&self) -> bool {
        match &self.entry {
            EntryPoint::Vertex(..) => false,
            EntryPoint::Fragment(..) => true,
            EntryPoint::Compute(..) => false,
        }
    }

    pub fn is_compute(&self) -> bool {
        match &self.entry {
            EntryPoint::Vertex(..) => false,
            EntryPoint::Fragment(..) => false,
            EntryPoint::Compute(..) => true,
        }
    }

    pub fn get_entry_str(&self) -> Option<&str> {
        self.entry.as_str()
    }
}

impl HasLabel for Shader {
    fn label(&self) -> &str {
        &self.module.label
    }
}

impl Deref for Shader {
    type Target = wgpu::ShaderModule;

    fn deref(&self) -> &Self::Target {
        &self.module.inner
    }
}

impl fmt::Debug for Shader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let module = &*self.module;
        module.fmt(f)
    }
}

#[derive(Debug)]
struct ShaderModule {
    label: StaticStr,
    inner: wgpu::ShaderModule,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EntryPoint {
    Vertex(StaticStr),
    Fragment(StaticStr),
    Compute(StaticStr),
}

impl EntryPoint {
    pub fn vertex<T>(ident: T) -> Self
    where
        T: Into<StaticStr>,
    {
        EntryPoint::Vertex(ident.into())
    }

    pub fn fragment<T>(ident: T) -> Self
    where
        T: Into<StaticStr>,
    {
        EntryPoint::Fragment(ident.into())
    }

    pub fn compute<T>(ident: T) -> Self
    where
        T: Into<StaticStr>,
    {
        EntryPoint::Compute(ident.into())
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Vertex(s) => (!s.is_empty()).then_some(s),
            Self::Fragment(s) => (!s.is_empty()).then_some(s),
            Self::Compute(s) => (!s.is_empty()).then_some(s),
        }
    }
}
