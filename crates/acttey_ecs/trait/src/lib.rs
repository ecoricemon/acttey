pub trait Component {}

pub trait Entity {
    /// DO NOT call me!
    /// This is an internal validation function.
    fn validate();
}
