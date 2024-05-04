use super::{component::ComponentKey, resource::ResourceKey};
use crate::{
    ds::{generational::GenQueue, vec::OptVec},
    top::canvas::CanvasPack,
};
use std::{fmt::Debug, ops::Deref, sync::Arc};

/// A structure containing [`WaitQueue`]s for each component, resource, and entity.
/// Their indices are the same with the ones from the data containers.
/// For instance, [`EntityDict`](super::entity::EntityDict) manages indices for each entity and component.
/// This structure follows the exact same indices by taking them over.
#[derive(Debug)]
pub struct WaitQueuePack {
    ent_queues: OptVec<OptVec<WaitQueue>>,
    res_queues: OptVec<WaitQueue>,
}

impl WaitQueuePack {
    pub fn new() -> Self {
        Self {
            ent_queues: OptVec::new(),
            res_queues: OptVec::new(),
        }
    }

    /// Takes O(n) time.
    pub fn is_all_queue_empty(&self) -> bool {
        self.ent_queues
            .iter_occupied()
            .flat_map(|(_, cols)| cols.iter_occupied())
            .all(|(_, col)| col.is_empty())
            && self
                .res_queues
                .iter_occupied()
                .all(|(_, col)| col.is_empty())
    }

    /// Makes wait queues for an entity.
    /// This makes a queue at the position `enti`.
    /// Also, it makes inner queues for components of the entity as much as `ncol`.
    /// If there was a queue at the position, it will be dropped first.
    pub fn init_entity_queue(&mut self, enti: usize, ncol: usize) {
        // Prepares wait queues for columns.
        let mut cols = OptVec::new();
        for _ in 0..ncol {
            cols.push(Some(WaitQueue::new()));
        }
        self.ent_queues.extend_set(enti, cols);
    }

    #[inline]
    pub fn init_resource_queue(&mut self, index: usize) {
        self.res_queues.extend_set(index, WaitQueue::new());
    }

    pub(super) fn enqueue(&mut self, wait: &mut WaitRequest) -> bool {
        wait.clean = if wait.clean {
            self.enqueue_begin(wait)
        } else {
            self.enqueue_again(wait)
        };
        wait.clean
    }

    pub(super) fn dequeue(&mut self, wait: &WaitRequest) {
        // Pops from *read* or *write* wait queues.
        fn inner_comp(ent_queues: &mut OptVec<OptVec<WaitQueue>>, indices: &[(usize, usize)]) {
            for (enti, coli) in indices.iter().cloned() {
                let comp_queues = ent_queues.get_mut(enti).unwrap();
                let queue = comp_queues.get_mut(coli).unwrap();
                queue.pop();
            }
        }

        // Pops from *res_read* or *res_write* wait queues.
        fn inner_res(res_queues: &mut OptVec<WaitQueue>, indices: &[usize]) {
            for resi in indices.iter().cloned() {
                let queue = res_queues.get_mut(resi).unwrap();
                queue.pop();
            }
        }

        // Pops from *ent_write* wait queues.
        fn inner_ent(ent_queues: &mut OptVec<OptVec<WaitQueue>>, indices: &[usize]) {
            for enti in indices.iter().cloned() {
                let comp_queues = ent_queues.get_mut(enti).unwrap();
                for (_, queue) in comp_queues.iter_occupied_mut() {
                    queue.pop();
                }
            }
        }

        // Dequeues component read & write requests from their wait queues.
        inner_comp(&mut self.ent_queues, &wait.read);
        inner_comp(&mut self.ent_queues, &wait.write);

        // Dequeues resource read & write requests from their wait queues.
        inner_res(&mut self.res_queues, &wait.res_read);
        inner_res(&mut self.res_queues, &wait.res_write);

        // Dequeues entity write requests from their wait queues.
        inner_ent(&mut self.ent_queues, &wait.ent_write);
    }

    fn enqueue_begin(&mut self, wait: &mut WaitRequest) -> bool {
        // Pushes *read* or *write* into their corresponding wait queues.
        fn inner_comp(
            ent_queues: &mut OptVec<OptVec<WaitQueue>>,
            indices: &[(usize, usize)],
            rw: RW,
            try_again: &mut Vec<(u64, usize, usize)>,
        ) -> bool {
            let mut all_available = true;
            for (enti, coli) in indices.iter().cloned() {
                let comp_queues = ent_queues.get_mut(enti).unwrap();
                let queue = comp_queues.get_mut(coli).unwrap();
                let target_gen = queue.push(rw);
                if target_gen != queue.gen() {
                    try_again.push((target_gen, enti, coli));
                    all_available = false;
                }
            }
            all_available
        }

        // Pushes *res_read* or *res_write* into their corresponding wait queues.
        fn inner_res(
            res_queues: &mut OptVec<WaitQueue>,
            indices: &[usize],
            rw: RW,
            try_again: &mut Vec<(u64, usize)>,
        ) -> bool {
            let mut all_available = true;
            for resi in indices.iter().cloned() {
                let queue = res_queues.get_mut(resi).unwrap();
                let target_gen = queue.push(rw);
                if target_gen != queue.gen() {
                    try_again.push((target_gen, resi));
                    all_available = false;
                }
            }
            all_available
        }

        // Pushes *ent_write* into their corresponding wait queues.
        fn inner_ent(
            ent_queues: &mut OptVec<OptVec<WaitQueue>>,
            indices: &[usize],
            try_again: &mut Vec<(u64, usize, usize)>,
        ) -> bool {
            let mut all_available = true;
            for enti in indices.iter().cloned() {
                let comp_queues = ent_queues.get_mut(enti).unwrap();
                for (coli, queue) in comp_queues.iter_occupied_mut() {
                    let target_gen = queue.push(RW::Write);
                    if target_gen != queue.gen() {
                        try_again.push((target_gen, enti, coli));
                        all_available = false;
                    }
                }
            }
            all_available
        }

        let mut res = true;

        // Enqueues component read & write requests into their wait queues.
        res &= inner_comp(
            &mut self.ent_queues,
            &wait.read,
            RW::Read,
            &mut wait.read_again,
        );
        res &= inner_comp(
            &mut self.ent_queues,
            &wait.write,
            RW::Write,
            &mut wait.write_again,
        );

        // Enqueues resource read & write requests into their wait queues.
        res &= inner_res(
            &mut self.res_queues,
            &wait.res_read,
            RW::Read,
            &mut wait.res_read_again,
        );
        res &= inner_res(
            &mut self.res_queues,
            &wait.res_write,
            RW::Write,
            &mut wait.res_write_again,
        );

        // Enqueues entity write requests into their wait queues.
        res &= inner_ent(
            &mut self.ent_queues,
            &wait.ent_write,
            &mut wait.ent_write_again,
        );

        res
    }

    fn enqueue_again(&mut self, wait: &mut WaitRequest) -> bool {
        // Checks *reads_again*, *writes_again*, or *ent_write* one more time.
        fn inner_comp(
            ent_queues: &OptVec<OptVec<WaitQueue>>,
            try_again: &mut Vec<(u64, usize, usize)>,
        ) -> bool {
            while let Some((target_gen, enti, coli)) = try_again.pop() {
                let comp_queues = ent_queues.get(enti).unwrap();
                let queue = comp_queues.get(coli).unwrap();
                if target_gen != queue.gen() {
                    try_again.push((target_gen, enti, coli));
                    return false;
                }
            }
            true
        }

        // Checks *res_reads_again* or *res_writes_again* one more time.
        fn inner_res(res_queues: &OptVec<WaitQueue>, try_again: &mut Vec<(u64, usize)>) -> bool {
            while let Some((target_gen, resi)) = try_again.pop() {
                let queue = res_queues.get(resi).unwrap();
                if target_gen != queue.gen() {
                    try_again.push((target_gen, resi));
                    return false;
                }
            }
            true
        }

        // Checks all requests once again.
        inner_comp(&self.ent_queues, &mut wait.read_again)
            && inner_comp(&self.ent_queues, &mut wait.write_again)
            && inner_res(&self.res_queues, &mut wait.res_read_again)
            && inner_res(&self.res_queues, &mut wait.res_write_again)
            && inner_comp(&self.ent_queues, &mut wait.ent_write_again)
    }
}

impl Default for WaitQueuePack {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct WaitQueue(GenQueue<(RW, usize)>);

impl WaitQueue {
    const fn new() -> Self {
        Self(GenQueue::new())
    }

    fn push(&mut self, rw: RW) -> u64 {
        if rw == RW::Read {
            if let Some(back) = self.0.back_mut() {
                if back.0 == RW::Read {
                    back.1 += 1;
                    return self.gen() + (self.len() - 1) as u64;
                }
            }
        }
        self.0.push_back((rw, 1));
        self.gen() + (self.len() - 1) as u64
    }

    fn pop(&mut self) {
        let front = self.0.front_mut().unwrap();
        if front.1 > 1 {
            front.1 -= 1;
        } else {
            self.0.pop_front();
        }
    }
}

impl Deref for WaitQueue {
    type Target = GenQueue<(RW, usize)>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum WaitNotifyType {
    Comp(ComponentKey),
    Res(ResourceKey),
    Ent(Arc<str>),
}

/// Read or write
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum RW {
    Read,
    Write,
}

#[derive(Debug)]
pub(super) struct WaitRequest {
    /// *Sorted and deduplicated* wait queue indices to read-only components.
    /// Tuple is an index pair of a specific entity-column pair.
    pub(super) read: Vec<(usize, usize)>,

    /// Not ready [`Self::read`] and their target generations.
    pub(super) read_again: Vec<(u64, usize, usize)>,

    /// *Sorted and deduplicated* wait queue indices to writable components.
    /// Tuple is an index pair of a specific entity-column pair.
    pub(super) write: Vec<(usize, usize)>,

    /// Not ready [`Self::write`] and their target generations.
    pub(super) write_again: Vec<(u64, usize, usize)>,

    /// *Sorted and deduplicated* wait queue indices to read-only resources.
    pub(super) res_read: Vec<usize>,

    /// Not ready [`Self::res_read`] and their target generations.
    pub(super) res_read_again: Vec<(u64, usize)>,

    /// *Sorted and deduplicated* wait queue indices to writable resources.
    pub(super) res_write: Vec<usize>,

    /// Not ready [`Self::res_write`] and their target generations.
    pub(super) res_write_again: Vec<(u64, usize)>,

    /// *Sorted and deduplicated* wait queue indices to writable entity containers.
    pub(super) ent_write: Vec<usize>,

    /// Not ready [`Self::ent_write`] and their target generations.
    pub(super) ent_write_again: Vec<(u64, usize, usize)>,

    /// If it's not clean, it's in the middle of availability check.
    /// So we can continue it rather than doing it from the beginning.
    pub(super) clean: bool,
}

impl WaitRequest {
    pub(super) const fn new() -> Self {
        Self {
            read: Vec::new(),
            read_again: Vec::new(),
            write: Vec::new(),
            write_again: Vec::new(),
            res_read: Vec::new(),
            res_read_again: Vec::new(),
            res_write: Vec::new(),
            res_write_again: Vec::new(),
            ent_write: Vec::new(),
            ent_write_again: Vec::new(),
            clean: true,
        }
    }
}
