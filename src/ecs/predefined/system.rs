use crate::{
    app::input::Input,
    ecs::{
        predefined::resource::TimeStamp,
        query::{Query, QueryMut, ResQuery, ResQueryMut},
        system::System,
    },
    render::RenderResource,
};

pub struct Resized;
impl System for Resized {
    type Ref = ();
    type Mut = ();
    type ResRef = ();
    type ResMut = (Input, RenderResource);

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        _m: <Self::Mut as QueryMut>::Output,
        _rr: <Self::ResRef as ResQuery>::Output,
        rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        let (input, render) = rm;
        for handle in input.iter_resized() {
            crate::log!("resize from {handle}");
        }
        if input.iter_resized().count() > 0 {
            render.resize_drawables();
        }
    }
}

pub struct ClearInput;
impl System for ClearInput {
    type Ref = ();
    type Mut = ();
    type ResRef = ();
    type ResMut = Input;

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        _m: <Self::Mut as QueryMut>::Output,
        _rr: <Self::ResRef as ResQuery>::Output,
        rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        rm.clear();
    }
}

pub struct Render;
impl System for Render {
    type Ref = ();
    type Mut = ();
    type ResRef = TimeStamp;
    type ResMut = RenderResource;

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        _m: <Self::Mut as QueryMut>::Output,
        rr: <Self::ResRef as ResQuery>::Output,
        rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        let time = rr;
        let render = rm;
        render.render(0);
    }
}
