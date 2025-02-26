use super::{to_code::ConstructWgslCode, util};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Attributes(pub Vec<Attribute>);

impl Attributes {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    /// Retrieves the index of the exactly matched attribute.
    pub fn find_attribute(&self, attr: &Attribute) -> Option<usize> {
        util::find_index(self.0.iter(), attr, Some)
    }

    /// Searches partially matched attribute and returns its index.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn find_attribute_partial(&self, outer: &str, inner: Option<&str>) -> Option<usize> {
        if let Some(inner) = inner {
            self.find_attribute(&Attribute::from((outer, inner)))
        } else {
            self.0
                .iter()
                .enumerate()
                .find_map(|(i, attr)| attr.is_same_outer2(outer).then_some(i))
        }
    }

    /// Tests if there's an attribute that exactly matches with the given attribute.
    pub fn contains_attribute(&self, attr: &Attribute) -> bool {
        self.find_attribute(attr).is_some()
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn contains_attribute_partial(&self, outer: &str, inner: Option<&str>) -> bool {
        self.find_attribute_partial(outer, inner).is_some()
    }

    /// Tries to remove the attribute that matches with the given attribute.
    pub fn remove_attribute(&mut self, attr: &Attribute) -> Option<Attribute> {
        self.find_attribute(attr).map(|i| self.0.remove(i))
    }
}

impl std::ops::Deref for Attributes {
    type Target = Vec<Attribute>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Attributes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// 11. Attributes
// https://www.w3.org/TR/WGSL/#attributes
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Align(u32),
    Binding(u32),
    Builtin(BuiltinValue),
    Const,
    // Diagnostic,
    Group(u32),
    Id(u32),
    // Interpolate,
    Invariant,
    Location(u32),
    MustUse,
    Size(u32),
    WorkgroupSize(u32, u32, u32),
    Vertex,
    Fragment,
    Compute,
    MyId(String),
}

impl Attribute {
    /// Tests if the given attribute has the same variant.
    pub fn is_same_outer(&self, rhs: &Attribute) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(rhs)
    }

    /// Tests if this instance is a variant equivalent to the given string.
    pub fn is_same_outer2(&self, rhs: &str) -> bool {
        self.is_same_outer(&Attribute::from(rhs))
    }

    /// Tests if this instance is custom attribute like MyId.
    pub fn is_my_attr(&self) -> bool {
        matches!(self, Self::MyId(..))
    }

    /// Returns a string corresponding to this variant.
    pub fn outer(&self) -> &str {
        match self {
            Self::Align(..) => "align",
            Self::Binding(..) => "binding",
            Self::Builtin(..) => "builtin",
            Self::Const => "const",
            // Self::Diagnostic => unimplemented!()
            Self::Group(..) => "group",
            Self::Id(..) => "id",
            // Self::Interpolate => unimplemented!()
            Self::Invariant => "invariant",
            Self::Location(..) => "location",
            Self::MustUse => "must_use",
            Self::Size(..) => "size",
            Self::WorkgroupSize(..) => "workgroup_size",
            Self::Vertex => "vertex",
            Self::Fragment => "fragment",
            Self::Compute => "compute",
            Self::MyId(..) => "ID",
        }
    }

    /// Returns a string converted from inner value.
    pub fn inner(&self) -> Option<String> {
        match self {
            Self::Align(v) => Some(v.to_string()),
            Self::Binding(v) => Some(v.to_string()),
            Self::Builtin(v) => Some(v.wgsl_code()),
            Self::Const => None,
            // Self::Diagnostic => unimplemented!()
            Self::Group(v) => Some(v.to_string()),
            Self::Id(v) => Some(v.to_string()),
            // Self::Interpolate => unimplemented!()
            Self::Invariant => None,
            Self::Location(v) => Some(v.to_string()),
            Self::MustUse => None,
            Self::Size(v) => Some(v.to_string()),
            Self::WorkgroupSize(x, y, z) => Some(format!("{x}, {y}, {z}")),
            Self::Vertex => None,
            Self::Fragment => None,
            Self::Compute => None,
            Self::MyId(v) => Some(v.clone()),
        }
    }

    /// Returns u32 inner value only, otherwise, returns None.
    /// Use [`Self::inner`] if you want string type.
    pub fn inner_u32(&self) -> Option<u32> {
        match self {
            Self::Align(v) => Some(*v),
            Self::Binding(v) => Some(*v),
            Self::Builtin(_v) => None,
            Self::Const => None,
            // Self::Diagnostic => unimplemented!()
            Self::Group(v) => Some(*v),
            Self::Id(v) => Some(*v),
            // Self::Interpolate => unimplemented!()
            Self::Invariant => None,
            Self::Location(v) => Some(*v),
            Self::MustUse => None,
            Self::Size(v) => Some(*v),
            Self::WorkgroupSize(x, y, z) => {
                if *y == 1 && *z == 1 {
                    Some(*x)
                } else {
                    None
                }
            }
            Self::Vertex => None,
            Self::Fragment => None,
            Self::Compute => None,
            Self::MyId(_v) => None,
        }
    }

    /// Sets inner value.
    pub fn set_inner(&mut self, inner: &str) {
        match self {
            Self::Align(v) => *v = inner.parse().unwrap(),
            Self::Binding(v) => *v = inner.parse().unwrap(),
            Self::Builtin(v) => *v = BuiltinValue::from(inner),
            Self::Const => (),
            // Self::Diagnostic => unimplemented!()
            Self::Group(v) => *v = inner.parse().unwrap(),
            Self::Id(v) => *v = inner.parse().unwrap(),
            // Self::Interpolate => unimplemented!()
            Self::Invariant => (),
            Self::Location(v) => *v = inner.parse().unwrap(),
            Self::MustUse => (),
            Self::Size(v) => *v = inner.parse().unwrap(),
            Self::WorkgroupSize(x, y, z) => {
                let xyz = util::str_into_xyz(inner).unwrap();
                *x = xyz[0];
                *y = xyz[1];
                *z = xyz[2];
            }
            Self::Vertex => (),
            Self::Fragment => (),
            Self::Compute => (),
            Self::MyId(v) => *v = String::from(inner),
        }
    }
}

impl ConstructWgslCode for Attribute {
    fn write_wgsl_string(&self, buf: &mut String) {
        match self {
            Self::Align(v)
            | Self::Binding(v)
            | Self::Group(v)
            | Self::Id(v)
            | Self::Location(v)
            | Self::Size(v) => {
                buf.push('@');
                if !self.is_my_attr() {
                    buf.push_str(self.outer());
                }
                buf.push('(');
                buf.push_str(&v.to_string());
                buf.push(')');
            }
            Self::Builtin(v) => {
                buf.push('@');
                if !self.is_my_attr() {
                    buf.push_str(self.outer());
                }
                buf.push('(');
                v.write_wgsl_string(buf);
                buf.push(')');
            }
            Self::Const
            | Self::Invariant
            | Self::MustUse
            | Self::Vertex
            | Self::Fragment
            | Self::Compute => {
                buf.push('@');
                if !self.is_my_attr() {
                    buf.push_str(self.outer());
                }
            }
            Self::WorkgroupSize(x, y, z) => {
                buf.push('@');
                if !self.is_my_attr() {
                    buf.push_str(self.outer());
                }
                buf.push_str(&format!("({x},{y},{z})"));
            }
            Self::MyId(..) => {}
        };
        // Self::Diagnostic => unimplemented!()
        // Self::Interpolate => unimplemented!()
    }
}

impl From<(&str, &str)> for Attribute {
    fn from(value: (&str, &str)) -> Self {
        match value {
            ("align", v) => Self::Align(v.parse().unwrap()),
            ("binding", v) => Self::Binding(v.parse().unwrap()),
            ("builtin", v) => Self::Builtin(BuiltinValue::from(v)),
            ("Const", _) => Self::Const, // Conflict with 'const'
            // ("diagnostic", _) => unimplemented!()
            ("group", v) => Self::Group(v.parse().unwrap()),
            ("id", v) => Self::Id(v.parse().unwrap()),
            // ("interpolate", _) => unimplemented!()
            ("invariant", _) => Self::Invariant,
            ("location", v) => Self::Location(v.parse().unwrap()),
            ("must_use", _) => Self::MustUse,
            ("size", v) => Self::Size(v.parse().unwrap()),
            ("workgroup_size", v) => {
                let xyz = util::str_into_xyz(v).unwrap();
                Self::WorkgroupSize(xyz[0], xyz[1], xyz[2])
            }
            ("vertex", _) => Self::Vertex,
            ("fragment", _) => Self::Fragment,
            ("compute", _) => Self::Compute,
            ("ID", v) => Self::MyId(v.to_owned()),
            _ => panic!("{}({}) is not a WGSL attribute", value.0, value.1),
        }
    }
}

impl From<(&str, u32)> for Attribute {
    fn from(value: (&str, u32)) -> Self {
        match value {
            ("align", v) => Self::Align(v),
            ("binding", v) => Self::Binding(v),
            ("builtin", v) => Self::Builtin(BuiltinValue::from(v.to_string().as_str())),
            ("Const", _) => Self::Const,
            // ("diagnostic", _) => unimplemented!()
            ("group", v) => Self::Group(v),
            ("id", v) => Self::Id(v),
            // ("interpolate", _) => unimplemented!()
            ("invariant", _) => Self::Invariant,
            ("location", v) => Self::Location(v),
            ("must_use", _) => Self::MustUse,
            ("size", v) => Self::Size(v),
            ("workgroup_size", x) => Self::WorkgroupSize(x, 1, 1),
            ("vertex", _) => Self::Vertex,
            ("fragment", _) => Self::Fragment,
            ("compute", _) => Self::Compute,
            ("ID", v) => Self::MyId(v.to_string()),
            _ => panic!("{}({}) is not a WGSL attribute", value.0, value.1),
        }
    }
}

impl From<&str> for Attribute {
    fn from(value: &str) -> Self {
        match value {
            "align" => Self::Align(Default::default()),
            "binding" => Self::Binding(Default::default()),
            "builtin" => Self::Builtin(Default::default()),
            "Const" => Self::Const,
            //("diagnostic" => unimplemented!()
            "group" => Self::Group(Default::default()),
            "id" => Self::Id(Default::default()),
            // "interpolate" => unimplemented!()
            "invariant" => Self::Invariant,
            "location" => Self::Location(Default::default()),
            "must_use" => Self::MustUse,
            "size" => Self::Size(Default::default()),
            "workgroup_size" => Self::WorkgroupSize(1, 1, 1),
            "vertex" => Self::Vertex,
            "fragment" => Self::Fragment,
            "compute" => Self::Compute,
            "ID" => Self::MyId(Default::default()),
            _ => panic!("{value} is not a WGSL attribute"),
        }
    }
}

// 12.3.1.1. Built-in Inputs and Outputs
// https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinValue {
    #[default]
    VertexIndex, // Vertex input
    InstanceIndex,        // Vertex input
    Position,             // Vertex output(mandatory) & Fragment input(Not mandatory)
    FrontFacing,          // Fragment input
    FragDepth,            // Fragment output
    SampleIndex,          // Fragment input
    SampleMask,           // Fragment input & output
    LocalInvocationId,    // Compute input
    LocalInvocationIndex, // Compute input
    GlobalInvocationId,   // Compute input
    WorkgroupId,          // Compute input
    NumWorkgroups,        // Compute input
}

impl ConstructWgslCode for BuiltinValue {
    fn write_wgsl_string(&self, buf: &mut String) {
        match self {
            Self::VertexIndex => buf.push_str("vertex_index"),
            Self::InstanceIndex => buf.push_str("instance_index"),
            Self::Position => buf.push_str("position"),
            Self::FrontFacing => buf.push_str("front_facing"),
            Self::FragDepth => buf.push_str("frag_depth"),
            Self::SampleIndex => buf.push_str("sample_index"),
            Self::SampleMask => buf.push_str("sample_mask"),
            Self::LocalInvocationId => buf.push_str("local_invocation_id"),
            Self::LocalInvocationIndex => buf.push_str("local_invocation_index"),
            Self::GlobalInvocationId => buf.push_str("global_invocation_id"),
            Self::WorkgroupId => buf.push_str("workgroup_id"),
            Self::NumWorkgroups => buf.push_str("num_workgroups"),
        }
    }
}

impl From<&str> for BuiltinValue {
    fn from(value: &str) -> Self {
        match value {
            "vertex_index" => Self::VertexIndex,
            "instance_index" => Self::InstanceIndex,
            "position" => Self::Position,
            "front_facing" => Self::FrontFacing,
            "frag_depth" => Self::FragDepth,
            "sample_index" => Self::SampleIndex,
            "sample_mask" => Self::SampleMask,
            "local_invocation_id" => Self::LocalInvocationId,
            "local_invocation_index" => Self::LocalInvocationIndex,
            "global_invocation_id" => Self::GlobalInvocationId,
            "workgroup_id" => Self::WorkgroupId,
            "num_workgroups" => Self::NumWorkgroups,
            _ => panic!("{value} is not a built-in inputs or outputs"),
        }
    }
}
