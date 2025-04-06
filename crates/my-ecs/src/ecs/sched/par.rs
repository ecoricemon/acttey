use super::{
    ctrl::SUB_CONTEXT,
    task::{ParTask, ParTaskHolder, TaskId},
};
use crate::global;
use rayon::iter::{
    IndexedParallelIterator, ParallelIterator,
    plumbing::{
        Consumer, Folder, Producer, ProducerCallback, Reducer, UnindexedConsumer, UnindexedProducer,
    },
};

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/rayon-core/src/lib.rs#L851
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub(crate) struct FnContext {
    migrated: bool,
}

impl FnContext {
    pub(super) const MIGRATED: Self = Self { migrated: true };
    pub(super) const NOT_MIGRATED: Self = Self { migrated: false };
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/src/iter/plumbing/mod.rs#L255C1-L255C18
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct Splitter {
    splits: usize,
}

impl Splitter {
    fn new() -> Option<Self> {
        let ptr = SUB_CONTEXT.get();
        (!ptr.is_dangling()).then(|| {
            // Safety: `ptr` is a valid pointer.
            Self {
                splits: unsafe { ptr.as_ref().get_comm().num_siblings() },
            }
        })
    }

    fn try_split(&mut self, migrated: bool) -> bool {
        // rayon implementation.
        //
        // If a task is migrated, i.e. took it from another worker's local queue,
        // that could mean that some workers are hungry for tasks.
        // Therefore, it would be good for keeping processors busy
        // to split this task into enough small pieces.
        // if migrated {
        //     let num_workers = unsafe { SUB_CONTEXT.get().as_ref().siblings.len() };
        //     self.splits = num_workers.max(self.splits / 2);
        //     true
        // } else {
        //     let res = self.splits > 0;
        //     self.splits /= 2;
        //     res
        // }

        // my implementation.
        //
        // Currently, I need to optimize work distribution.
        // Meanwhile, this could hide poor performance.
        if !migrated {
            self.splits /= 2;
        }
        self.splits > 0
    }
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/src/iter/plumbing/mod.rs#L293
#[derive(Debug, Clone, Copy)]
struct LengthSplitter {
    inner: Splitter,
    min: usize,
}

impl LengthSplitter {
    fn new(min: usize, max: usize, len: usize) -> Option<Self> {
        let mut inner = Splitter::new()?;
        let min_splits = len / max.max(1);
        inner.splits = inner.splits.max(min_splits);

        Some(Self {
            inner,
            min: min.max(1),
        })
    }

    fn try_split(&mut self, len: usize, migrated: bool) -> bool {
        len / 2 >= self.min && self.inner.try_split(migrated)
    }
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/src/iter/plumbing/mod.rs#L350
fn bridge<I, C>(par_iter: I, consumer: C) -> C::Result
where
    I: IndexedParallelIterator,
    C: Consumer<I::Item>,
{
    global::stat::increase_parallel_task_count();

    let len = par_iter.len();
    return par_iter.with_producer(Callback { len, consumer });

    struct Callback<C> {
        len: usize,
        consumer: C,
    }

    impl<C, I> ProducerCallback<I> for Callback<C>
    where
        C: Consumer<I>,
    {
        type Output = C::Result;
        fn callback<P>(self, producer: P) -> C::Result
        where
            P: Producer<Item = I>,
        {
            bridge_producer_consumer(self.len, producer, self.consumer)
        }
    }
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/src/iter/plumbing/mod.rs#L390
fn bridge_producer_consumer<P, C>(len: usize, producer: P, consumer: C) -> C::Result
where
    P: Producer,
    C: Consumer<P::Item>,
{
    const MIGRATED: bool = false;
    let res =
        if let Some(splitter) = LengthSplitter::new(producer.min_len(), producer.max_len(), len) {
            helper(len, MIGRATED, splitter, producer, consumer)
        } else {
            helper_no_split(producer, consumer)
        };
    return res;

    // === Internal helper functions ===

    fn helper<P, C>(
        len: usize,
        migrated: bool,
        mut splitter: LengthSplitter,
        producer: P,
        consumer: C,
    ) -> C::Result
    where
        P: Producer,
        C: Consumer<P::Item>,
    {
        if consumer.full() {
            consumer.into_folder().complete()
        } else if splitter.try_split(len, migrated) {
            let mid = len / 2;
            let (l_producer, r_producer) = producer.split_at(mid);
            let (l_consumer, r_consumer, reducer) = consumer.split_at(mid);
            let (l_result, r_result) = join_context(
                |f_cx: FnContext| helper(mid, f_cx.migrated, splitter, l_producer, l_consumer),
                |f_cx: FnContext| {
                    helper(len - mid, f_cx.migrated, splitter, r_producer, r_consumer)
                },
            );
            reducer.reduce(l_result, r_result)
        } else {
            producer.fold_with(consumer.into_folder()).complete()
        }
    }

    fn helper_no_split<P, C>(producer: P, consumer: C) -> C::Result
    where
        P: Producer,
        C: Consumer<P::Item>,
    {
        if consumer.full() {
            consumer.into_folder().complete()
        } else {
            producer.fold_with(consumer.into_folder()).complete()
        }
    }
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/src/iter/plumbing/mod.rs#L445
#[allow(dead_code)] // For future use
fn bridge_unindexed<P, C>(producer: P, consumer: C) -> C::Result
where
    P: UnindexedProducer,
    C: UnindexedConsumer<P::Item>,
{
    global::stat::increase_parallel_task_count();
    let splitter = Splitter::new().unwrap();
    bridge_unindexed_producer_consumer(false, splitter, producer, consumer)
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/src/iter/plumbing/mod.rs#L454
fn bridge_unindexed_producer_consumer<P, C>(
    migrated: bool,
    mut splitter: Splitter,
    producer: P,
    consumer: C,
) -> C::Result
where
    P: UnindexedProducer,
    C: UnindexedConsumer<P::Item>,
{
    if consumer.full() {
        consumer.into_folder().complete()
    } else if splitter.try_split(migrated) {
        match producer.split() {
            (l_producer, Some(r_producer)) => {
                let (reducer, l_consumer, r_consumer) =
                    (consumer.to_reducer(), consumer.split_off_left(), consumer);
                let bridge = bridge_unindexed_producer_consumer;
                let (l_result, r_result) = join_context(
                    |f_cx: FnContext| bridge(f_cx.migrated, splitter, l_producer, l_consumer),
                    |f_cx: FnContext| bridge(f_cx.migrated, splitter, r_producer, r_consumer),
                );
                reducer.reduce(l_result, r_result)
            }
            (producer, None) => producer.fold_with(consumer.into_folder()).complete(),
        }
    } else {
        producer.fold_with(consumer.into_folder()).complete()
    }
}

// ref: https://github.com/rayon-rs/rayon/blob/7543ed40c9a017dee32b3dc72b3ae819820e8366/rayon-core/src/join/mod.rs#L115
fn join_context<L, R, Lr, Rr>(l_f: L, r_f: R) -> (Lr, Rr)
where
    L: FnOnce(FnContext) -> Lr + Send,
    R: FnOnce(FnContext) -> Rr + Send,
    Lr: Send,
    Rr: Send,
{
    let cx = unsafe { SUB_CONTEXT.get().as_ref() };

    let r_holder = ParTaskHolder::new(r_f);
    let r_task = unsafe { ParTask::new(&r_holder) };
    let r_task_id = TaskId::Parallel(r_task);

    // Puts 'Right task' into local queue then notifies it to another worker.
    //
    // TODO: Optimize.
    // compare to rayon, too many steal operations take place.
    // The more unnecessary steal operations, the poorer performance.
    // I guess this frequent notification is one of the reasons.
    // Anyway, I mitigated it by reducing split count.
    // See `Splitter::try_split`.
    cx.get_comm().push_parallel_task(r_task);
    cx.get_comm().signal().sub().notify_one();

    // Executes 'Left task'.
    #[cfg(not(target_arch = "wasm32"))]
    let l_res = {
        let executor = std::panic::AssertUnwindSafe(move || l_f(FnContext::NOT_MIGRATED));
        match std::panic::catch_unwind(executor) {
            Ok(l_res) => l_res,
            Err(payload) => {
                // Panicked in `Left task`.
                // But we need to hold `Right task` until it's finished
                // if it was stolen by another worker.
                if let Some(task) = cx.get_comm().pop_local() {
                    debug_assert_eq!(task.id(), r_task_id);
                } else {
                    while !r_holder.is_executed() {
                        std::thread::yield_now();
                    }
                }
                std::panic::resume_unwind(payload);
            }
        }
    };

    // In web, we don't have a way to hold `Right task` when `Left task` panics.
    #[cfg(target_arch = "wasm32")]
    let l_res = l_f(FnContext::NOT_MIGRATED);

    // If we could find a task from the local queue, it must be 'Right task',
    // because the queue is LIFO fashion.
    if let Some(task) = cx.get_comm().pop_local() {
        debug_assert_eq!(task.id(), r_task_id);
        let wid = cx.get_comm().worker_id();
        r_task.execute(wid, FnContext::NOT_MIGRATED);
    } else {
        // We couldn't find a task from local queue.
        // That means that another worker has stolen 'Right task'.
        // While we wait for 'Right task' to be finished by the another worker,
        // steals some tasks and executes them.
        while !r_holder.is_executed() {
            let mut steal = cx.get_comm().search();
            cx.work(&mut steal);
            // TODO: Busy waiting if it failed to steal tasks.
        }
    }

    match unsafe { r_holder.return_or_panic_unchecked() } {
        Ok(r_res) => (l_res, r_res),
        Err(payload) => std::panic::resume_unwind(payload),
    }
}

/// A trait for wrapping Rayon's parallel iterators in [`EcsPar`] in order to
/// intercept function call to a Rayon API then to execute them in the ECS
/// context.
pub trait IntoEcsPar: ParallelIterator {
    /// Wraps Rayon's parallel iterator in [`EcsPar`].
    ///
    /// `EcsPar` calls an ECS function to make use of ECS workers instead of
    /// Rayon's workers.
    #[inline]
    fn into_ecs_par(self) -> EcsPar<Self> {
        EcsPar(self)
    }

    /// Implementations must call [`bridge`] or [`bridge_unindexed`] instead of
    /// [`rayon::iter::plumbing::bridge`] or
    /// [`rayon::iter::plumbing::bridge_unindexed`].
    #[doc(hidden)]
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>;
}

impl<I: IndexedParallelIterator> IntoEcsPar for I {
    #[inline]
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        // Intercepts.
        bridge(self, consumer)
    }
}

/// A wrapper type of Rayon's parallel iterator.
///
/// Rayon's parallel iterator basically uses its own worker registry. It means
/// that Rayon will spawn new workers regardless of living workers in ECS
/// instance, which is a behavior you may not want. To use workers of ECS
/// instance instead, just wrap the Rayon's parallel iterator in this wrapper.
/// Then, this wrapper will intercept calls to Rayon's functions.
///
/// # Limitation
///
/// This wrapper currently requires implmentation of
/// [`IndexedParallelIterator`].
#[derive(Clone)]
#[repr(transparent)]
pub struct EcsPar<I>(pub I);

impl<I: IntoEcsPar> ParallelIterator for EcsPar<I> {
    type Item = I::Item;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        // Intercepts
        IntoEcsPar::drive_unindexed(self.0, consumer)
    }
}

impl<I> IndexedParallelIterator for EcsPar<I>
where
    I: IntoEcsPar + IndexedParallelIterator,
{
    #[inline]
    fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    fn drive<C: Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        // Intercepts
        bridge(self, consumer)
    }

    #[inline]
    fn with_producer<CB: ProducerCallback<Self::Item>>(self, callback: CB) -> CB::Output {
        self.0.with_producer(callback)
    }
}

#[cfg(test)]
mod tests {
    /// Wraps rayon's parallel iterators in an interceptor.
    #[test]
    fn test_into_ecs_par() {
        use super::*;
        use rayon::iter::IntoParallelIterator;

        // Array
        let iter: rayon::array::IntoIter<i32, 2> = [0, 1].into_par_iter();
        let _ecs_iter = iter.into_ecs_par();
        // Range
        let iter: rayon::range::Iter<i32> = (0..2).into_par_iter();
        let _ecs_iter = iter.into_ecs_par();
        // Slice
        let iter: rayon::slice::Iter<'_, i32> = [0, 1][..].into_par_iter();
        let _ecs_iter = iter.into_ecs_par();
        // Zip
        let range_iter0: rayon::range::Iter<i32> = (0..2).into_par_iter();
        let range_iter1: rayon::range::Iter<i32> = (0..2).into_par_iter();
        let zip_iter = range_iter0.zip(range_iter1);
        let _ecs_iter = zip_iter.into_ecs_par();
    }
}
