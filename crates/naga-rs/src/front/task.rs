use std::{
    cell::Cell,
    collections::BTreeMap,
    mem,
    ops::{AddAssign, Index, IndexMut, SubAssign},
    rc::Rc,
};

#[derive(Debug)]
pub struct TaskQueue<T> {
    front: BTreeMap<TaskId, Task<T>>,
    back: BTreeMap<TaskId, Task<T>>,
    head: TaskId,
    tail: TaskId,
}

impl<T> TaskQueue<T> {
    pub const fn new() -> Self {
        Self {
            front: BTreeMap::new(),
            back: BTreeMap::new(),
            head: TaskId(0),
            tail: TaskId(1),
        }
    }

    pub fn len(&self) -> usize {
        self.front.len() + self.back.len()
    }

    pub fn push_front(&mut self, value: T) -> TaskId {
        self.push_task_front(Task {
            value: Some(value),
            latter: Rc::new(()),
            me: Rc::new(()),
        })
    }

    pub fn push_front_with_dependency(&mut self, value: T, latter: TaskId) -> TaskId {
        let latter_me = Rc::clone(&self[latter].me);
        self.push_task_front(Task {
            value: Some(value),
            latter: latter_me,
            me: Rc::new(()),
        })
    }

    pub fn push_task_front(&mut self, task: Task<T>) -> TaskId {
        let tid = self.head;
        self.head -= 1;
        self.front.insert(tid, task);
        tid
    }

    pub fn push_back(&mut self, value: T) -> TaskId {
        self.push_task_back(Task {
            value: Some(value),
            latter: Rc::new(()),
            me: Rc::new(()),
        })
    }

    pub fn push_back_with_dependency(&mut self, value: T, prior: TaskId) -> TaskId {
        let task = Task {
            value: Some(value),
            latter: Rc::new(()),
            me: Rc::new(()),
        };
        self[prior].latter = Rc::clone(&task.me);
        self.push_task_back(task)
    }

    pub fn push_task_back(&mut self, task: Task<T>) -> TaskId {
        let tid = self.tail;
        self.tail += 1;
        self.back.insert(tid, task);
        tid
    }

    pub fn pop(&mut self) -> Option<Task<T>> {
        while let Some((tid, task)) = self.front.pop_first() {
            if Rc::strong_count(&task.me) > 1 {
                self.back.insert(tid, task);
            } else {
                return Some(task);
            }
        }

        mem::swap(&mut self.front, &mut self.back);

        while let Some((tid, task)) = self.front.pop_first() {
            if Rc::strong_count(&task.me) > 1 {
                self.back.insert(tid, task);
            } else {
                return Some(task);
            }
        }

        None
    }

    /// Adds dependency between two tasks so that `prior` task will be followed
    /// by `latter` task.
    ///
    /// In other words, `latter` depends on `prior`, which means `latter` will
    /// not be popped from the queue until `prior` is popped and dropped.
    ///
    /// `latter` can depend on multiple 'prior's, but 'prior' is related to only
    /// one `latter`.
    pub fn add_dependency(&mut self, prior: TaskId, latter: TaskId) {
        let latter_me = Rc::clone(&self[latter].me);
        self[prior].latter = latter_me;
    }
}

impl<T> Index<TaskId> for TaskQueue<T> {
    type Output = Task<T>;

    fn index(&self, index: TaskId) -> &Self::Output {
        if let Some(task) = self.front.get(&index) {
            task
        } else {
            self.back.get(&index).unwrap()
        }
    }
}

impl<T> IndexMut<TaskId> for TaskQueue<T> {
    fn index_mut(&mut self, index: TaskId) -> &mut Self::Output {
        if let Some(task) = self.front.get_mut(&index) {
            task
        } else {
            self.back.get_mut(&index).unwrap()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TaskId(i32);

impl AddAssign<i32> for TaskId {
    fn add_assign(&mut self, rhs: i32) {
        self.0 += rhs;
    }
}

impl SubAssign<i32> for TaskId {
    fn sub_assign(&mut self, rhs: i32) {
        self.0 -= rhs;
    }
}

#[derive(Debug)]
pub struct Task<T> {
    value: Option<T>,
    latter: Rc<()>,
    me: Rc<()>,
}

impl<T> Task<T> {
    pub fn take(&mut self) -> T {
        self.value.take().unwrap()
    }

    pub fn into_value(mut self) -> T {
        self.value.take().unwrap()
    }
}
