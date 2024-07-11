use super::{
    canvas::{CanvasHandle, CanvasPack, OffCanvas},
    context::{Gpu, RenderContext},
    shaders::res::Shader,
};
use crate::{
    common::AppHasher,
    ds::{
        refs::WithRc,
        share::{DebugLock, SharedVec, SharedVecItem, SharedVecOrigin},
    },
    util::{Or, Rect},
};
use desc::*;
use my_ecs::{ds::prelude::*, util::prelude::*};
use res::*;
use std::{
    cell::Cell,
    collections::HashMap,
    hash::{Hash, Hasher},
    mem,
    num::NonZeroU32,
    ptr::NonNull,
    rc::Rc,
};

thread_local! {
    // For debugging.
    // For now, all shared variables in this module share this counter to detect invalid mutable borrows.
    // This may change in the future.
    #[cfg(debug_assertions)]
    pub(crate) static FRAG_MOD_DEBUG_LOCK: Cell<i32> = const { Cell::new(0) };
}

#[derive(Debug)]
pub struct FragmentStateManager {
    context: Rc<RenderContext>,
    gpu: Rc<Gpu>,

    /// Surfaces, color textures, and depth stencil textures.
    texs: TexturePack,

    /// Pointer to the vector inside [`Self::org_frag_states`].
    pub(crate) frag_states: SharedVec<FragmentStateRc, AppHasher>,

    /// Pointer to the vector inside [`Self::org_depth_stencil_states`].
    pub(crate) depth_stencil_states: SharedVec<DepthStencilStateRc, AppHasher>,

    /// Actual vector of [`FragmentState`].
    /// This field is not accessed and just keeps vector alive.
    org_frag_states: SharedVecOrigin<FragmentStateRc, AppHasher>,

    /// Actual vector of [`DepthStencilState`].
    /// This field is not accessed and just keeps vector alive.
    org_depth_stencil_states: SharedVecOrigin<DepthStencilStateRc, AppHasher>,
}

impl FragmentStateManager {
    pub(crate) fn new(context: Rc<RenderContext>, gpu: Rc<Gpu>) -> Self {
        let texs = TexturePack::new(Rc::clone(&context), Rc::clone(&gpu));
        let (frag_states, org_frag_states) = SharedVec::new();
        let (depth_stencil_states, org_depth_stencil_states) = SharedVec::new();

        Self {
            context,
            gpu,
            texs,
            frag_states,
            depth_stencil_states,
            org_frag_states,
            org_depth_stencil_states,
        }
    }

    pub(crate) fn register_canvas(
        &mut self,
        sel: String,
        canvas: OffCanvas,
        format: Option<wgpu::TextureFormat>,
    ) {
        self.texs.register_canvas(sel, canvas, format)
    }

    /// # Panics
    ///
    /// See [`SharedVecItem::new`].
    pub(crate) fn get_fragment_state(&self, index: usize) -> FragmentStateRef {
        DebugLock::new(
            SharedVecItem::new(self.frag_states, index),
            #[cfg(debug_assertions)]
            &FRAG_MOD_DEBUG_LOCK,
        )
    }

    /// # Panics
    ///
    /// See [`SharedVecItem::new`].
    pub(crate) fn get_depth_stencil_state(&self, index: usize) -> DepthStencilStateRef {
        DebugLock::new(
            SharedVecItem::new(self.depth_stencil_states, index),
            #[cfg(debug_assertions)]
            &FRAG_MOD_DEBUG_LOCK,
        )
    }
}

impl StoreFragmentState for FragmentStateManager {
    fn add_fragment_state(&mut self, desc: FragmentStateDesc) -> FragmentStateRef {
        // Creates `FragmentState`.
        let state = FragmentState::from_descriptor(desc);

        // Appends the fragment state.
        let index = self.frag_states.add(WithRc::new(state));

        self.get_fragment_state(index)
    }

    fn remove_fragment_state(
        &mut self,
        state: FragmentStateRef,
    ) -> Result<FragmentState, FragmentStateRef> {
        let state = state.into_inner();
        self.frag_states
            .remove(state)
            .map(FragmentStateRc::into_inner)
            .map_err(|state| {
                DebugLock::new(
                    state,
                    #[cfg(debug_assertions)]
                    &FRAG_MOD_DEBUG_LOCK,
                )
            })
    }
}

impl StoreDepthStencilState for FragmentStateManager {
    fn add_depth_stencil_state(&mut self, desc: DepthStencilStateDesc) -> DepthStencilStateRef {
        // Creates depth stencil state.
        let state = DepthStencilState::from_descriptor(desc);

        // Appends the depth stencil state.
        let index = self.depth_stencil_states.add(WithRc::new(state));

        self.get_depth_stencil_state(index)
    }

    fn remove_depth_stencil_state(
        &mut self,
        state: DepthStencilStateRef,
    ) -> Result<DepthStencilState, DepthStencilStateRef> {
        let state = state.into_inner();
        self.depth_stencil_states
            .remove(state)
            .map(DepthStencilStateRc::into_inner)
            .map_err(|state| {
                DebugLock::new(
                    state,
                    #[cfg(debug_assertions)]
                    &FRAG_MOD_DEBUG_LOCK,
                )
            })
    }
}

impl StoreTexture for FragmentStateManager {
    fn get_surface(&self, sel: &str) -> Option<SurfaceRef> {
        self.texs.get_surface(sel)
    }

    /// # Panics
    ///
    /// Panics if canvas/surface hasn't been registered with the given selectors.
    fn configure_surface(&mut self, sel: &str, conf: wgpu::SurfaceConfiguration) {
        let mut item = self.get_surface(sel).unwrap();
        let mut surf = item.borrow_mut();
        surf.configure(conf);
    }

    fn add_texture(&mut self, desc: wgpu::TextureDescriptor<'static>) -> TextureRef {
        let tex = Texture::new(Rc::clone(&self.gpu), desc);
        self.texs.append_texture(tex)
    }

    fn remove_texture(&mut self, tex: TextureRef) -> Result<Texture, TextureRef> {
        self.texs.remove_texture(tex).map(TextureRc::into_inner)
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Execution layer is responsible for executing render or compute passes.
pub(crate) mod exe {
    use super::*;

    #[derive(Debug)]
    pub(crate) struct ColorAttachmentPack {
        /// Color attachments.
        //
        // This field is writable, but Some and None status never change.
        // So each element address won't change too.
        colors: Box<[Option<ColorAttachment>]>,

        /// [`wgpu::RenderPassColorAttachment`] list for a render pass.
        //
        // wpug's attachment has a reference field to a texture view in `Self::colors`.
        // In other words, we need to explicitly declare lifetime bound to self.
        // In order to elide to do it, we use AnyVec.
        // That's fine because element addresses of `Self::colors` won't change.
        attachs: AnyVec,
    }

    impl ColorAttachmentPack {
        /// Creates template of color attachment list.
        pub(crate) fn new(colors: Box<[Option<ColorAttachment>]>) -> Self {
            // Creates wgpu's color attachment list.
            let mut attachs = AnyVec::new(tinfo!(Option<wgpu::RenderPassColorAttachment>));
            for _ in colors.iter() {
                attachs.push::<Option<wgpu::RenderPassColorAttachment>>(None);
            }

            Self { colors, attachs }
        }

        pub(crate) fn as_color_attachments(
            &mut self,
        ) -> &[Option<wgpu::RenderPassColorAttachment<'_>>] {
            self.reset_attachments();
            self.attachs.as_slice()
        }

        pub(crate) fn present(&mut self) {
            for color in self.colors.iter_mut() {
                if let Some(color) = color {
                    color.present();
                }
            }
        }

        fn reset_attachments(&mut self) {
            for (i, color) in self.colors.iter_mut().enumerate() {
                if let Some(color) = color {
                    if color.dirty {
                        // Safety: Local variable attach is being inerted into `self.attachs`.
                        let mut attach = color.as_attachment();
                        unsafe {
                            let ptr = NonNull::new_unchecked(&mut attach as *mut _ as *mut u8);
                            self.attachs.set_raw_unchecked(i, ptr);
                        }
                        mem::forget(attach);

                        // Clears dirty flag.
                        color.dirty = false;
                    }
                }
            }
        }
    }

    #[derive(Debug)]
    pub(crate) struct ColorAttachment {
        pub(crate) surf_or_tex: Or<SurfaceRef, TextureRef>,

        pub(crate) desc: wgpu::TextureViewDescriptor<'static>,

        pub(crate) ops: ColorOps,

        /// Generated surface texture if `surf_or_tex` is a surface.
        pub(crate) surf_tex: Option<wgpu::SurfaceTexture>,

        /// Generated texture view.
        pub(crate) view: wgpu::TextureView,

        pub(crate) dirty: bool,
    }

    impl ColorAttachment {
        pub(crate) fn new(
            surf_or_tex: Or<SurfaceRef, TextureRef>,
            desc: wgpu::TextureViewDescriptor<'static>,
            ops: ColorOps,
        ) -> Self {
            let (surf_tex, view) = Self::create_view(&surf_or_tex, &desc);

            Self {
                surf_or_tex,
                desc,
                ops,
                surf_tex,
                view,
                dirty: true,
            }
        }

        pub(crate) fn configure(
            &mut self,
            desc: Option<wgpu::TextureViewDescriptor<'static>>,
            ops: Option<ColorOps>,
        ) {
            if let Some(desc) = desc {
                self.desc = desc;
                self.recreate_view();
            }
            if let Some(ops) = ops {
                self.ops = ops;
            }
        }

        pub(crate) fn resize(&mut self, size: Rect<NonZeroU32>) {
            // Resizes surface or recreates texture.
            match &mut self.surf_or_tex {
                Or::A(surf) => {
                    let mut surf = surf.borrow_mut();
                    surf.resize(size);
                }
                Or::B(tex) => {
                    let mut tex = tex.borrow_mut();
                    tex.resize(size);
                }
            }

            // Recreates texture view.
            self.recreate_view();
        }

        pub(crate) fn present(&mut self) {
            if let Some(surf_tex) = self.surf_tex.take() {
                surf_tex.present();

                // For the next frame.
                self.recreate_view();
            }
        }

        pub(crate) fn as_attachment(&self) -> wgpu::RenderPassColorAttachment {
            wgpu::RenderPassColorAttachment {
                view: &self.view,
                resolve_target: None, /* TODO */
                ops: self.ops.operations(),
            }
        }

        fn recreate_view(&mut self) {
            let (surf_tex, view) = Self::create_view(&self.surf_or_tex, &self.desc);
            self.surf_tex = surf_tex;
            self.view = view;
            self.dirty = true;
        }

        fn create_view(
            surf_or_tex: &Or<SurfaceRef, TextureRef>,
            desc: &wgpu::TextureViewDescriptor<'static>,
        ) -> (Option<wgpu::SurfaceTexture>, wgpu::TextureView) {
            match &surf_or_tex {
                Or::A(surf) => {
                    let tex = surf.as_ref().get_current_texture();
                    let view = tex.texture.create_view(&desc);
                    (Some(tex), view)
                }
                Or::B(tex) => {
                    let view = tex.as_ref().create_view(&desc);
                    (None, view)
                }
            }
        }
    }

    #[derive(Debug)]
    pub(crate) struct DepthStencilAttachment {
        pub(crate) tex: TextureRef,

        pub(crate) desc: wgpu::TextureViewDescriptor<'static>,

        pub(crate) ops: DepthStencilOps,

        /// Generated texture view.
        pub(crate) view: wgpu::TextureView,
    }

    impl DepthStencilAttachment {
        pub(crate) fn new(
            tex: TextureRef,
            desc: wgpu::TextureViewDescriptor<'static>,
            ops: DepthStencilOps,
        ) -> Self {
            let view = tex.as_ref().create_view(&desc);

            Self {
                tex,
                desc,
                ops,
                view,
            }
        }

        pub(crate) fn configure(
            &mut self,
            desc: Option<wgpu::TextureViewDescriptor<'static>>,
            ops: Option<DepthStencilOps>,
        ) {
            if let Some(desc) = desc {
                self.view = self.tex.as_ref().create_view(&desc);
                self.desc = desc;
            }
            if let Some(ops) = ops {
                self.ops = ops;
            }
        }

        pub(crate) fn resize(&mut self, size: Rect<NonZeroU32>) {
            // Recreates texture.
            let mut tex = self.tex.borrow_mut();
            tex.resize(size);
            drop(tex);

            // Recreates texture view.
            self.view = self.tex.as_ref().create_view(&self.desc);
        }

        pub(crate) fn as_attachment(&self) -> wgpu::RenderPassDepthStencilAttachment {
            wgpu::RenderPassDepthStencilAttachment {
                view: &self.view,
                depth_ops: self.ops.depth_operations(),
                stencil_ops: self.ops.stencil_operations(),
            }
        }
    }

    /// Operation that will be performed on a color attachment.
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct ColorOps(wgpu::Operations<wgpu::Color>);

    impl ColorOps {
        const DEFAULT: Self = Self(wgpu::Operations {
            load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
            store: wgpu::StoreOp::Store,
        });

        pub fn new(color: wgpu::Operations<wgpu::Color>) -> Self {
            let is_valid_range = matches!(
                color,
                wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color {r, g, b, a}),
                    ..
                }
                if
                (0.0..=1.0).contains(&r) &&
                (0.0..=1.0).contains(&g) &&
                (0.0..=1.0).contains(&b) &&
                (0.0..=1.0).contains(&a)
            );
            assert!(is_valid_range);

            Self(color)
        }

        pub fn operations(&self) -> wgpu::Operations<wgpu::Color> {
            self.0
        }
    }

    impl Default for ColorOps {
        fn default() -> Self {
            Self::DEFAULT
        }
    }

    // floating values must be in from 0.0 to 1.0 range, so it's fine to implement Eq.
    // We checks it out at DepthStencilOps::new().
    impl Eq for ColorOps {}

    /// Operation that will be performed on a depth stencil attachment.
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct DepthStencilOps {
        depth: Option<wgpu::Operations<f32>>,
        stencil: Option<wgpu::Operations<u32>>,
    }

    impl DepthStencilOps {
        const DEFAULT: Self = Self {
            depth: Some(wgpu::Operations {
                load: wgpu::LoadOp::Clear(1.0),
                store: wgpu::StoreOp::Store,
            }),
            stencil: None,
        };

        pub fn new(
            depth: Option<wgpu::Operations<f32>>,
            stencil: Option<wgpu::Operations<u32>>,
        ) -> Self {
            let is_valid_range = matches!(
                depth,
                Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(v),
                    ..
                })
                if (0.0..=1.0).contains(&v)
            );
            assert!(is_valid_range);

            Self { depth, stencil }
        }

        pub fn depth_operations(&self) -> Option<wgpu::Operations<f32>> {
            self.depth
        }

        pub fn stencil_operations(&self) -> Option<wgpu::Operations<u32>> {
            self.stencil
        }
    }

    impl Default for DepthStencilOps {
        fn default() -> Self {
            Self::DEFAULT
        }
    }

    // floating values must be in from 0.0 to 1.0 range, so it's fine to implement Eq.
    // We checks it out at DepthStencilOps::new().
    impl Eq for DepthStencilOps {}
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Description layer is responsible for describing GPU state using something like pipeline.
pub(crate) mod desc {
    use super::*;

    /// Fragment state insertion and removal interfaces.
    pub trait StoreFragmentState {
        fn add_fragment_state(&mut self, desc: FragmentStateDesc) -> FragmentStateRef;
        fn remove_fragment_state(
            &mut self,
            state: FragmentStateRef,
        ) -> Result<FragmentState, FragmentStateRef>;
    }

    /// Depth stencil state insertion and removal interfaces.
    pub trait StoreDepthStencilState {
        fn add_depth_stencil_state(&mut self, desc: DepthStencilStateDesc) -> DepthStencilStateRef;
        fn remove_depth_stencil_state(
            &mut self,
            state: DepthStencilStateRef,
        ) -> Result<DepthStencilState, DepthStencilStateRef>;
    }

    #[derive(Debug)]
    pub struct FragmentStateDesc {
        pub shader: Rc<Shader>,

        pub colors: Vec<Option<ColorTargetStateDesc>>,
    }

    #[derive(Debug)]
    pub struct ColorTargetStateDesc {
        pub surf_or_tex: Or<SurfaceRef, TextureRef>,

        /// If None, [`ColorTargetState::DEFAULT_COLOR_TARGET_STATE`] is adapted as default value.
        pub state: Option<wgpu::ColorTargetState>,
    }

    #[derive(Debug)]
    pub struct DepthStencilStateDesc {
        pub tex: TextureRef,

        /// If None, [`DepthStencilState::DEFAULT_DEPTH_STENCIL_STATE`] is adapted as default value.
        pub state: Option<wgpu::DepthStencilState>,
    }

    /// Writable reference to a fragment state.
    pub(crate) type FragmentStateRef =
        DebugLock<SharedVecItem<FragmentStateRc, AppHasher>, FragmentStateRc>;

    /// Shared fragment state.
    pub(crate) type FragmentStateRc = WithRc<FragmentState>;

    #[derive(Debug, Clone)]
    pub struct FragmentState {
        shader: Rc<Shader>,

        /// Color target states that contain surface or textures in them.
        //
        // This field is writable, but Some and None status never change.
        colors: Box<[Option<ColorTargetState>]>,

        /// Texture width and height.
        /// All textures including surface must have the same extent.
        //
        // See https://developer.mozilla.org/en-US/docs/Web/API/GPUCommandEncoder/beginRenderPass#validation
        size: Rect<NonZeroU32>,
    }

    impl FragmentState {
        pub(crate) fn new(shader: Rc<Shader>, colors: Box<[Option<ColorTargetState>]>) -> Self {
            // Shader must have fragment stage.
            assert!(shader.has_fragment_stage());

            // `colors` constructs a set of surface and textures.
            // The set is used in a render pass as well and they must have the same extent.
            let size = colors
                .iter()
                .filter_map(|opt| opt.as_ref().map(|color| color.size()))
                .next()
                .unwrap();

            // Do all color target states have the same size?
            // WebGPU will look into it, but we can warn about it in debug mode.
            #[cfg(debug_assertions)]
            {
                let is_same_size = colors.iter().all(|color| {
                    if let Some(color) = color {
                        color.size() == size
                    } else {
                        true
                    }
                });
                assert!(
                    is_same_size,
                    "all surface and textures in a fragment state must have the same extent"
                );
            }

            Self {
                shader,
                colors,
                size,
            }
        }

        pub(crate) fn from_descriptor(desc: FragmentStateDesc) -> Self {
            // Creates `ColorTargetState`.
            let colors = desc
                .colors
                .into_iter()
                .map(|color| color.map(ColorTargetState::from_descriptor))
                .collect::<Vec<_>>();

            Self::new(desc.shader, colors.into_boxed_slice())
        }

        pub(crate) fn width(&self) -> NonZeroU32 {
            self.size.width
        }

        pub(crate) fn height(&self) -> NonZeroU32 {
            self.size.height
        }

        pub(crate) fn size(&self) -> Rect<NonZeroU32> {
            self.size
        }

        pub(crate) fn create_color_target_states(&self) -> Vec<Option<wgpu::ColorTargetState>> {
            self.colors
                .iter()
                .map(|opt| opt.as_ref().map(|color| color.color_target_state()))
                .collect()
        }

        pub(crate) fn create_fragment_state<'a>(
            &'a self,
            targets: &'a [Option<wgpu::ColorTargetState>],
        ) -> wgpu::FragmentState {
            wgpu::FragmentState {
                module: &self.shader.get_module(),
                // Safety: We checked it out at Self::new().
                entry_point: unsafe { self.shader.get_fragment_stage().unwrap_unchecked() },
                targets,
            }
        }

        /// # Panics
        ///
        /// Panics if width or height is zero.
        pub(crate) fn resize(&mut self, size: Rect<NonZeroU32>) {
            self.size = size;

            for color in self.colors.iter_mut() {
                if let Some(color) = color {
                    // Zero size causes panic here.
                    color.resize(size);
                }
            }
        }
    }

    impl PartialEq for FragmentState {
        fn eq(&self, other: &Self) -> bool {
            let Self {
                shader: this_shader,
                colors: this_colors,
                size: _this_size, // size is dependent on colors, so no need to compare.
            } = self;

            let Self {
                shader: other_shader,
                colors: other_colors,
                size: _other_size,
            } = other;

            Shader::is_same(this_shader, other_shader) && this_colors == other_colors
        }
    }

    impl Eq for FragmentState {}

    impl Hash for FragmentState {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self {
                shader,
                colors,
                size: _size, // size is dependent on colors, so no need to compare.
            } = self;

            Shader::hash(shader, state);
            colors.hash(state);
        }
    }

    /// Writable reference to a depth stencil state.
    pub(crate) type DepthStencilStateRef =
        DebugLock<SharedVecItem<DepthStencilStateRc, AppHasher>, DepthStencilStateRc>;

    /// Shared depth stencil state.
    pub(crate) type DepthStencilStateRc = WithRc<DepthStencilState>;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct DepthStencilState {
        pub(crate) tex: TextureRef,

        /// Read only [`wgpu::DepthStencilState`] to build a pipeline.
        pub(crate) state: wgpu::DepthStencilState,
    }

    impl DepthStencilState {
        const DEFAULT_DEPTH_STENCIL_STATE: wgpu::DepthStencilState = wgpu::DepthStencilState {
            format: wgpu::TextureFormat::Depth24Plus, /* Not used */
            depth_write_enabled: true,
            depth_compare: wgpu::CompareFunction::Less,
            stencil: Self::DEFAULT_STENCIL_STATE,
            bias: Self::DEFAULT_DEPTH_BIAS_STATE,
        };

        /// Same with wgpu's.
        const DEFAULT_STENCIL_STATE: wgpu::StencilState = wgpu::StencilState {
            front: wgpu::StencilFaceState::IGNORE,
            back: wgpu::StencilFaceState::IGNORE,
            read_mask: 0,
            write_mask: 0,
        };

        /// Same with wgpu's.
        const DEFAULT_DEPTH_BIAS_STATE: wgpu::DepthBiasState = wgpu::DepthBiasState {
            constant: 0,
            slope_scale: 0.0,
            clamp: 0.0,
        };

        /// # Panics
        ///
        /// Panics if `tex` texture format is not the same as `state`.
        pub(crate) fn new(tex: TextureRef, state: Option<wgpu::DepthStencilState>) -> Self {
            let state = state.unwrap_or(wgpu::DepthStencilState {
                format: tex.as_ref().texture_format(),
                ..Self::DEFAULT_DEPTH_STENCIL_STATE
            });

            // Texture format must match.
            assert_eq!(tex.as_ref().texture_format(), state.format);

            Self { tex, state }
        }

        pub(crate) fn from_descriptor(desc: DepthStencilStateDesc) -> Self {
            Self::new(desc.tex, desc.state)
        }

        pub(crate) fn width(&self) -> NonZeroU32 {
            self.tex.as_ref().width()
        }

        pub(crate) fn height(&self) -> NonZeroU32 {
            self.tex.as_ref().height()
        }

        pub(crate) fn texture_format(&self) -> wgpu::TextureFormat {
            self.state.format
        }

        pub(crate) fn configure(&mut self, desc: wgpu::TextureDescriptor<'static>) {
            let mut tex = self.tex.borrow_mut();
            tex.configure(desc);
        }

        pub(crate) fn depth_stencil_state(&self) -> wgpu::DepthStencilState {
            self.state.clone()
        }

        /// # Panics
        ///
        /// Panics if width or height is zero.
        pub(crate) fn resize(&mut self, size: Rect<NonZeroU32>) {
            let mut tex = self.tex.borrow_mut();
            tex.resize(size);
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ColorTargetState {
        pub(crate) surf_or_tex: Or<SurfaceRef, TextureRef>,

        /// Read only [`wgpu::ColorTargetState`] to build a pipeline.
        pub(crate) state: wgpu::ColorTargetState,
    }

    impl ColorTargetState {
        const DEFAULT_COLOR_TARGET_STATE: wgpu::ColorTargetState = wgpu::ColorTargetState {
            format: wgpu::TextureFormat::Bgra8Unorm, /* Not used */
            blend: None,
            write_mask: wgpu::ColorWrites::ALL,
        };

        /// # Panics
        ///
        /// Panics if `surf` texture format is not the same as `state`.
        pub(crate) fn new(
            surf_or_tex: Or<SurfaceRef, TextureRef>,
            state: Option<wgpu::ColorTargetState>,
        ) -> Self {
            let format = match &surf_or_tex {
                Or::A(surf) => surf.as_ref().texture_format(),
                Or::B(tex) => tex.as_ref().texture_format(),
            };
            let state = state.unwrap_or(wgpu::ColorTargetState {
                format,
                ..Self::DEFAULT_COLOR_TARGET_STATE
            });

            // Texture format must match.
            assert_eq!(format, state.format);

            Self { surf_or_tex, state }
        }

        pub(crate) fn from_descriptor(desc: ColorTargetStateDesc) -> Self {
            Self::new(desc.surf_or_tex, desc.state)
        }

        pub(crate) fn width(&self) -> NonZeroU32 {
            match &self.surf_or_tex {
                Or::A(surf) => surf.as_ref().width(),
                Or::B(tex) => tex.as_ref().width(),
            }
        }

        pub(crate) fn height(&self) -> NonZeroU32 {
            match &self.surf_or_tex {
                Or::A(surf) => surf.as_ref().height(),
                Or::B(tex) => tex.as_ref().height(),
            }
        }

        pub(crate) fn size(&self) -> Rect<NonZeroU32> {
            match &self.surf_or_tex {
                Or::A(surf) => surf.as_ref().size(),
                Or::B(tex) => tex.as_ref().size(),
            }
        }

        pub(crate) fn texture_format(&self) -> wgpu::TextureFormat {
            self.state.format
        }

        pub(crate) fn configure(
            &mut self,
            desc: Or<wgpu::SurfaceConfiguration, wgpu::TextureDescriptor<'static>>,
        ) {
            match (&mut self.surf_or_tex, desc) {
                (Or::A(surf), Or::A(surf_conf)) => {
                    let mut surf = surf.borrow_mut();
                    surf.configure(surf_conf);
                }
                (Or::B(tex), Or::B(tex_desc)) => {
                    let mut tex = tex.borrow_mut();
                    tex.configure(tex_desc);
                }
                _ => panic!(),
            }
        }

        pub(crate) fn color_target_state(&self) -> wgpu::ColorTargetState {
            self.state.clone()
        }

        /// # Panics
        ///
        /// Panics if width or height is zero.
        pub(crate) fn resize(&mut self, size: Rect<NonZeroU32>) {
            match &mut self.surf_or_tex {
                Or::A(surf) => {
                    let mut surf = surf.borrow_mut();
                    surf.resize(size);
                }
                Or::B(tex) => {
                    let mut tex = tex.borrow_mut();
                    tex.resize(size);
                }
            }
        }
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Resource layer is responsible for holding GPU relative data.
pub(crate) mod res {
    use super::*;

    /// Texture insertion and removal interfaces.
    pub trait StoreTexture {
        fn get_surface(&self, sel: &str) -> Option<SurfaceRef>;
        fn configure_surface(&mut self, sel: &str, conf: wgpu::SurfaceConfiguration);
        fn add_texture(&mut self, desc: wgpu::TextureDescriptor<'static>) -> TextureRef;
        fn remove_texture(&mut self, tex: TextureRef) -> Result<Texture, TextureRef>;
    }

    #[derive(Debug)]
    pub(crate) struct TexturePack {
        context: Rc<RenderContext>,
        gpu: Rc<Gpu>,

        /// Pointer to the vector inside [`Self::org_surfs`].
        pub(crate) surfs: SharedVec<SurfaceRc, AppHasher>,

        /// Pointer to the vector inside [`Self::org_texs`].
        pub(crate) texs: SharedVec<TextureRc, AppHasher>,

        /// Actual vector of [`Surface`].
        /// This field is not accessed and just keeps vector alive.
        org_surfs: SharedVecOrigin<SurfaceRc, AppHasher>,

        /// Actual vector of [`Texture`] for color or depth/stencil textures.
        /// This field is not accessed and just keeps vector alive.
        org_texs: SharedVecOrigin<TextureRc, AppHasher>,

        /// Offscreen canvases.
        canvases: CanvasPack<OffCanvas>,

        /// Canvas has 1:1 relationship with surface.
        /// Using this map, we can find a specific surface from canvas' selectors.
        //
        // This map has `SharedVecItem` which has reference counter.
        // So that when we remove surface itself, we must remove this mapping first in order to reduce the counter.
        canvas_map: HashMap<CanvasHandle, SurfaceRef, AppHasher>,
    }

    impl TexturePack {
        pub(crate) fn new(context: Rc<RenderContext>, gpu: Rc<Gpu>) -> Self {
            let (surfs, org_surfs) = SharedVec::new();
            let (texs, org_texs) = SharedVec::new();

            Self {
                context,
                gpu,
                surfs,
                texs,
                org_surfs,
                org_texs,
                canvases: CanvasPack::new(),
                canvas_map: HashMap::default(),
            }
        }

        /// Registers canvas and creates a surface for that with the given configuration.
        /// If `conf` is None, then default configuration is adapted.
        /// You can configure the surface later by calling to [`Self::configure_surface`].
        pub(crate) fn register_canvas(
            &mut self,
            sel: String,
            canvas: OffCanvas,
            format: Option<wgpu::TextureFormat>,
        ) {
            // Inserts canvas.
            // Conflict selectors or handle causes panic here.
            let handle = canvas.handle();
            let canvas = Rc::new(canvas);
            let cloned = Rc::clone(&canvas);
            self.canvases.insert(sel, handle, canvas);

            // Creates and inserts surface.
            // Surface has relation with only one canvas.
            let surf = Surface::new(
                &self.context.instance,
                &self.context.adapter,
                Rc::clone(&self.gpu),
                cloned,
                format,
            );

            // Appends surface.
            let index = self.surfs.add(WithRc::new(surf));
            let item = DebugLock::new(
                SharedVecItem::new(self.surfs, index),
                #[cfg(debug_assertions)]
                &FRAG_MOD_DEBUG_LOCK,
            );

            // Adds mapping.
            self.canvas_map.insert(handle, item);
        }

        pub(crate) fn unregister_canvas(&mut self, sel: &str) -> Option<Rc<OffCanvas>> {
            // Removes mapping first to reduce the reference counter.
            let handle = self.canvases.selectors_to_handle(sel)?;
            let item = self.canvas_map.remove(&handle)?;

            // Removes surface.
            let item = item.into_inner();
            self.surfs.remove(item).unwrap();

            // Removes canvas.
            self.canvases.remove(sel)
        }

        pub(crate) fn get_surface(&self, sel: &str) -> Option<SurfaceRef> {
            let handle = self.canvases.selectors_to_handle(sel)?;
            self.canvas_map.get(&handle).cloned()
        }

        pub(crate) fn append_texture(&mut self, tex: Texture) -> TextureRef {
            let index = self.texs.add(WithRc::new(tex));

            DebugLock::new(
                SharedVecItem::new(self.texs, index),
                #[cfg(debug_assertions)]
                &FRAG_MOD_DEBUG_LOCK,
            )
        }

        pub(crate) fn remove_texture(&mut self, tex: TextureRef) -> Result<TextureRc, TextureRef> {
            let tex = tex.into_inner();
            self.texs.remove(tex).map_err(|tex| {
                DebugLock::new(
                    tex,
                    #[cfg(debug_assertions)]
                    &FRAG_MOD_DEBUG_LOCK,
                )
            })
        }
    }

    /// Writable reference to a surface.
    pub type SurfaceRef = DebugLock<SharedVecItem<SurfaceRc, AppHasher>, SurfaceRc>;

    /// Shared surface.
    pub type SurfaceRc = WithRc<Surface>;

    #[derive(Debug)]
    pub struct Surface {
        gpu: Rc<Gpu>,

        /// Canvas connected to the surface.
        canvas: Rc<OffCanvas>,

        /// Configuration for the surface.
        /// The configuration is set to default settings compatible with [`wgpu::Adapter`].
        /// You can modify it by calling to [`Self::configure`].
        conf: wgpu::SurfaceConfiguration,

        /// [`wgpu::Surface`] corresponding to WebGPU's [`GPUCanvasContext`](https://gpuweb.github.io/gpuweb/#canvas-context)
        surf: wgpu::Surface<'static>,
    }

    impl Surface {
        // Surface's texture format should be fixed due to the pipeline.
        // So the surface receives the format here and never modify it.
        pub(crate) fn new(
            instance: &wgpu::Instance,
            adapter: &wgpu::Adapter,
            gpu: Rc<Gpu>,
            canvas: Rc<OffCanvas>,
            format: Option<wgpu::TextureFormat>,
        ) -> Self {
            // Creates surface from the given canvas.
            let surf = instance
                .create_surface(wgpu::SurfaceTarget::OffscreenCanvas(
                    web_sys::OffscreenCanvas::clone(&canvas),
                ))
                .unwrap();

            // Creates default compatible surface configuration.
            let cap = surf.get_capabilities(adapter);
            let conf = wgpu::SurfaceConfiguration {
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                format: format.unwrap_or(cap.formats[0]),
                width: canvas.width(),
                height: canvas.height(),
                present_mode: cap.present_modes[0],
                alpha_mode: cap.alpha_modes[0],
                view_formats: Vec::new(),
                desired_maximum_frame_latency: 2,
            };

            // Configures the surface with the default configuration.
            // Users can reconfigure this later by calling to `Self::configure()`.
            Self::_configure(&gpu.device, &surf, &conf);

            Self {
                gpu,
                canvas,
                conf,
                surf,
            }
        }

        pub(crate) fn width(&self) -> NonZeroU32 {
            // Safety: Texture size must be nonzero. See `Self::_configure()`.
            unsafe { NonZeroU32::new_unchecked(self.canvas.width()) }
        }

        pub(crate) fn height(&self) -> NonZeroU32 {
            // Safety: Texture size must be nonzero. See `Self::_configure()`.
            unsafe { NonZeroU32::new_unchecked(self.canvas.height()) }
        }

        pub(crate) fn size(&self) -> Rect<NonZeroU32> {
            Rect {
                width: self.width(),
                height: self.height(),
            }
        }

        pub(crate) fn texture_format(&self) -> wgpu::TextureFormat {
            self.conf.format
        }

        /// Determines whether both references point to the same texture.
        pub(crate) fn addr_eq(this: &SurfaceRef, other: &SurfaceRef) -> bool {
            this.addr_eq(&*other)
        }

        /// Sets the given surface configuration and configures the surface using it.
        ///
        /// # Panics
        ///
        /// Panics if texture format is different from the current one.
        pub(crate) fn configure(&mut self, conf: wgpu::SurfaceConfiguration) {
            assert_eq!(self.conf.format, conf.format);

            Self::_configure(&self.gpu.device, &self.surf, &conf);
            self.conf = conf;
        }

        pub(crate) fn get_current_texture(&self) -> wgpu::SurfaceTexture {
            self.surf.get_current_texture().unwrap()
        }

        /// Configures the surface to have the given size
        /// if and only if the given size is different from the current size.
        ///
        /// # Panics
        ///
        /// Panics if width or height is zero.
        pub(crate) fn resize(&mut self, size: Rect<NonZeroU32>) {
            let width = size.width.get();
            let height = size.height.get();
            if self.conf.width != width || self.conf.height != height {
                self.conf.width = width;
                self.conf.height = height;
                Self::_configure(&self.gpu.device, &self.surf, &self.conf);
                crate::log!(
                    "[D] Surface::resize(): surface({}) has been resized to {:?}",
                    self.canvas.handle(),
                    size,
                );
            }
        }

        /// # Panics
        ///
        /// Panics if width or height is zero.
        fn _configure(
            device: &wgpu::Device,
            surf: &wgpu::Surface,
            conf: &wgpu::SurfaceConfiguration,
        ) {
            // Surface size must be greater than zero.
            assert!(conf.width > 0 && conf.height > 0);
            surf.configure(device, conf);
        }

        #[deprecated]
        pub(crate) fn create_texture_view(
            &self,
            desc: &wgpu::TextureViewDescriptor,
        ) -> (wgpu::SurfaceTexture, wgpu::TextureView) {
            let texture = self.surf.get_current_texture().unwrap();
            let view = texture.texture.create_view(desc);
            (texture, view)
        }
    }

    impl PartialEq for Surface {
        fn eq(&self, other: &Self) -> bool {
            let Self {
                gpu: _this_gpu,
                canvas: this_canvas,
                conf: this_conf,
                surf: _this_surf, // We can't compare wgpu's data directly.
            } = self;

            let Self {
                gpu: _other_gpu,
                canvas: other_canvas,
                conf: other_conf,
                surf: _other_surf,
            } = other;

            (Rc::ptr_eq(&this_canvas, &other_canvas) || *this_canvas == *other_canvas)
                && this_conf == other_conf
        }
    }

    impl Eq for Surface {}

    impl Hash for Surface {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self {
                gpu: _gpu,
                canvas,
                conf,
                surf: _surf, // We can't hash wgpu's data directly.
            } = self;

            (*canvas).hash(state);
            conf.hash(state);
        }
    }

    /// Writable reference to a texture.
    pub type TextureRef = DebugLock<SharedVecItem<TextureRc, AppHasher>, TextureRc>;

    /// Shared texture.
    pub type TextureRc = WithRc<Texture>;

    #[derive(Debug)]
    pub struct Texture {
        gpu: Rc<Gpu>,
        desc: wgpu::TextureDescriptor<'static>,
        tex: wgpu::Texture,
    }

    impl Texture {
        // Texture format should be fixed due to the pipeline.
        // So texture never modify it.
        pub(crate) fn new(gpu: Rc<Gpu>, desc: wgpu::TextureDescriptor<'static>) -> Self {
            let tex = Self::create_texture(&gpu.device, &desc);

            Self { gpu, desc, tex }
        }

        pub(crate) fn width(&self) -> NonZeroU32 {
            // Safety: Texture size must be nonzero. See `Self::create_texture()`.
            unsafe { NonZeroU32::new_unchecked(self.tex.width()) }
        }

        pub(crate) fn height(&self) -> NonZeroU32 {
            // Safety: Texture size must be nonzero. See `Self::create_texture()`.
            unsafe { NonZeroU32::new_unchecked(self.tex.height()) }
        }

        pub(crate) fn size(&self) -> Rect<NonZeroU32> {
            Rect {
                width: self.width(),
                height: self.height(),
            }
        }

        pub(crate) fn texture_format(&self) -> wgpu::TextureFormat {
            self.desc.format
        }

        /// Determines whether both references point to the same texture.
        pub(crate) fn addr_eq(this: &TextureRef, other: &TextureRef) -> bool {
            this.addr_eq(&*other)
        }

        /// # Panics
        ///
        /// Panics if texture format is different from the current one.
        pub(crate) fn configure(&mut self, desc: wgpu::TextureDescriptor<'static>) {
            assert_eq!(self.desc.format, desc.format);

            self.tex = Self::create_texture(&self.gpu.device, &desc);
            self.desc = desc;
        }

        pub(crate) fn create_view(&self, desc: &wgpu::TextureViewDescriptor) -> wgpu::TextureView {
            self.tex.create_view(desc)
        }

        /// Re-creates the texture to have the given size
        /// if and only if the given size is different from the current size.
        ///
        /// # Panics
        ///
        /// Panics if width or height is zero.
        pub(crate) fn resize(&mut self, size: Rect<NonZeroU32>) {
            let width = size.width.get();
            let height = size.height.get();
            if self.desc.size.width != width || self.desc.size.height != height {
                self.desc.size.width = width;
                self.desc.size.height = height;
                self.tex = Self::create_texture(&self.gpu.device, &self.desc);
            }
        }

        /// # Panics
        ///
        /// Panics if width or height is zero.
        fn create_texture(
            device: &wgpu::Device,
            desc: &wgpu::TextureDescriptor<'static>,
        ) -> wgpu::Texture {
            // Texture size must be greater than zero.
            assert!(desc.size.width > 0 && desc.size.height > 0);
            device.create_texture(desc)
        }
    }

    impl PartialEq for Texture {
        fn eq(&self, other: &Self) -> bool {
            let Self {
                gpu: _this_gpu,
                desc: this_desc,
                tex: _this_tex, // We can't compare wgpu's data directly.
            } = self;

            let Self {
                gpu: _other_gpu,
                desc: other_desc,
                tex: _other_tex,
            } = other;

            this_desc == other_desc
        }
    }

    impl Eq for Texture {}

    impl Hash for Texture {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self {
                gpu: _gpu,
                desc,
                tex: _tex, // We can't hash wgpu's data directly.
            } = self;

            desc.hash(state);
        }
    }
}
