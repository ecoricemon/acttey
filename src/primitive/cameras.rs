use crate::{
    impl_from_for_enum,
    primitive::{
        constant::radians,
        {matrix::Matrix4f, vector::Vector},
    },
    util::AsBytes,
};

#[derive(Debug)]
pub enum Camera {
    Perspective(PerspectiveCamera),
    Orthographic(OrthographicCamera),
}

impl_from_for_enum!(Camera, Perspective, PerspectiveCamera);
impl_from_for_enum!(Camera, Orthographic, OrthographicCamera);

impl Default for Camera {
    fn default() -> Self {
        Camera::Perspective(PerspectiveCamera::default())
    }
}

impl<'a> From<&'a Camera> for &'a [u8] {
    fn from(value: &'a Camera) -> Self {
        match value {
            Camera::Perspective(cam) => cam.into(),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct PerspectiveCamera {
    camera: Vector<f32, 3>,
    at: Vector<f32, 3>,
    up: Vector<f32, 3>,
    fovy: f32,
    aspect: f32, // width / height
    near: f32,
    far: f32,
    view: Matrix4f,
    proj: Matrix4f,
    pub view_proj: Matrix4f,
}

impl PerspectiveCamera {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    #[rustfmt::skip]
    pub fn look_at(camera: Vector<f32, 3>, at: Vector<f32, 3>, up: Vector<f32, 3>) -> Matrix4f {
        let forward = (camera - at).unit();
        let right = up.normal(forward);
        let up = forward.cross(right);
        Matrix4f::new([
            right.x(), up.x(), forward.x(), 0.0,
            right.y(), up.y(), forward.y(), 0.0,
            right.z(), up.z(), forward.z(), 0.0,
            -camera.dot(right), -camera.dot(up), -camera.dot(forward), 1.0
        ])
    }

    #[rustfmt::skip]
    pub fn project(fovy: f32, aspect: f32, near: f32, far: f32) -> Matrix4f {
        let cot_hfovy = 1.0 / (fovy / 2.0).tan();
        let n_f = near - far;
        Matrix4f::new([
            cot_hfovy / aspect, 0.0, 0.0, 0.0,
            0.0, cot_hfovy, 0.0, 0.0,
            0.0, 0.0, (near + far) / n_f, -1.0,
            0.0, 0.0, 2.0 * near * far / n_f, 0.0,
        ])
    }

    pub fn set_view(
        &mut self,
        camera: Option<(f32, f32, f32)>,
        at: Option<(f32, f32, f32)>,
        up: Option<(f32, f32, f32)>,
    ) {
        if let Some((x, y, z)) = camera {
            self.camera.set(x, y, z);
        }
        if let Some((x, y, z)) = at {
            self.at.set(x, y, z);
        }
        if let Some((x, y, z)) = up {
            self.up.set(x, y, z);
        }

        self.view = Self::look_at(self.camera, self.at, self.up);
        self.view_proj = &self.proj * &self.view;
    }

    pub fn set_proj(
        &mut self,
        fovy: Option<f32>,
        aspect: Option<f32>,
        near: Option<f32>,
        far: Option<f32>,
    ) {
        if let Some(x) = fovy {
            self.fovy = x;
        }
        if let Some(x) = aspect {
            self.aspect = x;
        }
        if let Some(x) = near {
            self.near = x;
        }
        if let Some(x) = far {
            self.far = x;
        }

        self.proj = Self::project(self.fovy, self.aspect, self.near, self.far);
        self.view_proj = &self.proj * &self.view;
    }
}

impl Default for PerspectiveCamera {
    fn default() -> Self {
        let camera = Vector::<f32, 3>::from([0.0, 0.0, 1.0]);
        let at = Vector::<f32, 3>::from(0.0);
        let up = Vector::<f32, 3>::from([0.0, 1.0, 0.0]);
        let fovy = radians::FRAC_PI_2;
        let aspect = 1.0;
        let near = 0.1;
        let far = 10.0;
        let view = Self::look_at(camera, at, up);
        let proj = Self::project(fovy, aspect, near, far);
        let view_proj = &proj * &view;

        Self {
            camera,
            at,
            up,
            fovy,
            aspect,
            near,
            far,
            view,
            proj,
            view_proj,
        }
    }
}

impl<'a> From<&'a PerspectiveCamera> for &'a [u8] {
    fn from(value: &'a PerspectiveCamera) -> Self {
        value.view_proj.as_bytes()
    }
}

// TODO: Not implemented yet.
#[derive(Debug)]
pub struct OrthographicCamera {
    pub dummy: PerspectiveCamera,
}
