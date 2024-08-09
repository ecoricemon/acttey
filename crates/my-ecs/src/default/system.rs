use crate::ecs::prelude::*;
use std::hash::BuildHasher;

/// A built-in default system group operation for repetitive systems.
/// Repetitive systems are systems that are executed every time when scheduling occurs,
/// while they are alive, in other words, not expired.
pub fn sgroup_op_repetitive<W, S>(sys: &mut SystemGroup<S>, mut cycle: Cycle<W, S>)
where
    S: BuildHasher + Default,
{
    cycle.run_cycle(sys);
    sys.tick();
}

/// A built-in default system group operation for reactive systems.
/// Reactive systems are systems that will be waken by the main waker when it needed.
/// Here's this function's assumptions,
/// - If main waker is not registered, does nothing.
/// - There's only one main waker.
/// - Waker listens to wait targets and wake systems to handle the targets.
/// - Systems will handle the targets and become inactive once they finished it up.
pub fn sgroup_op_reactive<W, S>(sys: &mut SystemGroup<S>, mut cycle: Cycle<W, S>)
where
    S: BuildHasher + Default,
{
    if sys.activate_system_num() > 0 {
        for i in 0.. {
            const MAX_ITER: usize = 10;
            debug_assert!(i < MAX_ITER, "too long chain detected");

            cycle.run_cycle(sys);
            sys.tick();

            if sys.activate_system_num() == 1 {
                break;
            }
        }
    }
}
