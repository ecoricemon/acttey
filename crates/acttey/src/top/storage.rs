use crate::{
    common::AppHasher,
    primitive::mesh::{
        Geometry, GeometryKey, Material, MaterialKey, Mesh, MeshKey, MeshPrimitive,
        SeparateGeometry,
    },
    util::key::{IntoObjectKey, ObjectKey},
};
use paste::paste;
use std::{
    any::Any,
    collections::HashMap,
    sync::{Arc, Mutex},
};

/// Holds common granular data which is randomly accessed.
/// The data can be accessed from multiple workers(threads).
#[derive(Debug)]
pub struct CommonStorage {
    /// Writable objects are shown below.
    /// - [`Geometry`]
    /// - [`Material`]
    /// - [`Mesh`]
    read_write: HashMap<ObjectKey, Arc<dyn Any + Send + Sync>, AppHasher>,

    // Read only objects are shown below.
    // - None for now
    _read: HashMap<ObjectKey, Arc<dyn Any + Send>, AppHasher>,
}

macro_rules! impl_basic_method {
    (rw, $name:ident, $ty:ty) => {
        paste! {
            pub(crate) fn [<insert_ $name>](&mut self, key: ObjectKey, value: $ty) -> Arc<Mutex<$ty>> {
                let value = Arc::new(Mutex::new(value));
                let cloned = Arc::clone(&value);

                // Inserts the value but prohibits removal by insertion.
                let old = self.read_write.insert(key, value);
                assert!(old.is_none(), "CommonStorage doesn't allow removal by insertion");

                cloned
            }

            pub(crate) fn [<strong_count_ $name>](&self, key: &ObjectKey) -> Option<usize> {
                self.read_write.get(key).map(|value| Arc::strong_count(value))
            }

            pub(crate) fn [<remove_ $name>](&mut self, key: &ObjectKey) -> Option<Arc<dyn Any + Send + Sync>> {
                // Prohibits removal currently referencing value.
                let strong_count = self.[<strong_count_ $name>](key);
                debug_assert!(matches!(strong_count, Some(cnt) if cnt == 1));

                self.read_write.remove(key)
            }

            pub(crate) fn [<get_ $name>](&mut self, key: &ObjectKey) -> Option<Arc<Mutex<$ty>>> {
                self.read_write.get(key).map(|value| {
                    let cloned = Arc::clone(value);
                    let value: Arc<Mutex<$ty>> = cloned.downcast().unwrap();
                    value
                })
            }
        }
    };
}

impl CommonStorage {
    pub(crate) fn new() -> Self {
        Self {
            read_write: HashMap::default(),
            _read: HashMap::default(),
        }
    }

    pub fn register_geometry<K, T>(&mut self, key: K, value: T)
    where
        K: IntoObjectKey,
        T: Into<SeparateGeometry>,
    {
        fn inner(this: &mut CommonStorage, key: ObjectKey, mut value: SeparateGeometry) {
            // Turns it into interleaved.
            value.sort();
            let mut value: Geometry = value.into();
            value.into_interleaved(None);

            // Inserts it.
            this.insert_geometry(key, value);
        }

        inner(self, key.into_key(), value.into());
    }

    pub fn register_material<K, T>(&mut self, key: K, value: T)
    where
        K: IntoObjectKey,
        T: Into<Material>,
    {
        self.insert_material(key.into_key(), value.into());
    }

    pub fn register_mesh<K, I, G, M>(&mut self, key: K, geo_mats: I)
    where
        K: IntoObjectKey,
        I: Iterator<Item = (G, M)>,
        G: IntoObjectKey,
        M: IntoObjectKey,
    {
        let prims = geo_mats
            .map(|(geo_key, mat_key)| {
                let geo_key = geo_key.into_key();
                let mat_key = mat_key.into_key();
                let geo = self.get_geometry(&geo_key).unwrap();
                let mat = self.get_material(&mat_key).unwrap();
                MeshPrimitive::new(
                    GeometryKey::new(geo_key),
                    MaterialKey::new(mat_key),
                    geo,
                    mat,
                )
            })
            .collect();

        let mesh = Mesh::from(prims);
        self.insert_mesh(key.into_key(), mesh);
    }

    // Implements insert/remove/get methods for geometry.
    impl_basic_method!(rw, geometry, Geometry);

    // Implements insert/remove/get methods for material.
    impl_basic_method!(rw, material, Material);

    // Implements insert/remove/get methods for mesh.
    impl_basic_method!(rw, mesh, Mesh);
}
