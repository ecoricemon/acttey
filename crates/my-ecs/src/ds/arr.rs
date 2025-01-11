use std::{
    alloc::Layout,
    mem,
    mem::MaybeUninit,
    ops::{Index, IndexMut},
    ptr, slice,
};

#[derive(Debug)]
pub struct ArrayDeque<T, const N: usize> {
    data: [MaybeUninit<T>; N],
    head: usize,
    len: usize,
}

impl<T, const N: usize> ArrayDeque<T, N> {
    pub const fn new() -> Self {
        Self {
            data: [const { MaybeUninit::uninit() }; N],
            head: 0,
            len: 0,
        }
    }

    pub const fn capacity(&self) -> usize {
        N
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub const fn is_full(&self) -> bool {
        self.len == N
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            let offset = self.offset(index);
            unsafe { Some(self.data[offset].assume_init_ref()) }
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            let offset = self.offset(index);
            unsafe { Some(self.data[offset].assume_init_mut()) }
        } else {
            None
        }
    }

    pub fn as_slices(&self) -> (&[T], &[T]) {
        debug_assert_eq!(
            Layout::new::<[MaybeUninit<T>; N]>(),
            Layout::new::<[T; N]>()
        );

        unsafe {
            let data_a = self.data.as_ptr().add(self.head) as *mut T;
            let len_a = self.len().min(N - self.head);
            let a = slice::from_raw_parts(data_a, len_a);

            let b = if self.head + self.len() > N {
                let data_b = self.data.as_ptr() as *mut T;
                let len_b = self.len() - len_a;
                slice::from_raw_parts(data_b, len_b)
            } else {
                &[]
            };

            (a, b)
        }
    }

    pub fn as_mut_slices(&mut self) -> (&mut [T], &mut [T]) {
        debug_assert_eq!(
            Layout::new::<[MaybeUninit<T>; N]>(),
            Layout::new::<[T; N]>()
        );

        unsafe {
            let data_a = self.data.as_mut_ptr().add(self.head) as *mut T;
            let len_a = self.len().min(N - self.head);
            let a = slice::from_raw_parts_mut(data_a, len_a);

            let b = if self.head + self.len() > N {
                let data_b = self.data.as_mut_ptr() as *mut T;
                let len_b = self.len() - len_a;
                slice::from_raw_parts_mut(data_b, len_b)
            } else {
                &mut []
            };

            (a, b)
        }
    }

    pub fn clear(&mut self) {
        self.drop_in_place();
        self.head = 0;
        self.len = 0;
    }

    pub fn push_back(&mut self, value: T) -> bool {
        if !self.is_full() {
            let tail = self.offset(self.len());
            self.data[tail].write(value);
            self.len += 1;
            true
        } else {
            false
        }
    }

    pub fn push_front(&mut self, value: T) -> bool {
        if !self.is_full() {
            self.head = self.offset(N - 1);
            self.data[self.head].write(value);
            self.len += 1;
            true
        } else {
            false
        }
    }

    pub fn pop_back(&mut self) -> Option<T> {
        if !self.is_empty() {
            self.len -= 1;
            let tail = self.offset(self.len());
            // Safety: It's not empty, so data[tail] must be initialized.
            unsafe { Some(self.data[tail].assume_init_read()) }
        } else {
            None
        }
    }

    pub fn pop_front(&mut self) -> Option<T> {
        if !self.is_empty() {
            // Safety: It's not empty, so data[head] must be initialized.
            let value = unsafe { self.data[self.head].assume_init_read() };
            self.head = (self.head + 1) % N;
            self.len -= 1;
            Some(value)
        } else {
            None
        }
    }

    pub fn back(&self) -> Option<&T> {
        if !self.is_empty() {
            let index = (self.head + self.len - 1) % N;
            // Safety: It's not empty, so data[index] must be initialized.
            unsafe { Some(self.data[index].assume_init_ref()) }
        } else {
            None
        }
    }

    pub fn front(&self) -> Option<&T> {
        if !self.is_empty() {
            // Safety: It's not empty, so data[head] must be initialized.
            unsafe { Some(self.data[self.head].assume_init_ref()) }
        } else {
            None
        }
    }

    const fn offset(&self, index: usize) -> usize {
        (self.head + index) % N
    }

    fn drop_in_place(&mut self) {
        let (a, b) = self.as_mut_slices();

        unsafe {
            ptr::drop_in_place(a as *mut _);
            ptr::drop_in_place(b as *mut _);
        }
    }
}

impl<T, const N: usize> Default for ArrayDeque<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const N: usize> Drop for ArrayDeque<T, N> {
    fn drop(&mut self) {
        if mem::needs_drop::<T>() {
            self.drop_in_place();
        }
    }
}

impl<T, const N: usize> Index<usize> for ArrayDeque<T, N> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("Out of bounds access")
    }
}

impl<T, const N: usize> IndexMut<usize> for ArrayDeque<T, N> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).expect("Out of bounds access")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arraydeque_new() {
        let arr: ArrayDeque<i32, 3> = ArrayDeque::new();
        assert_eq!(arr.len(), 0);
        assert!(arr.is_empty());
        assert!(!arr.is_full());
    }

    #[test]
    fn test_arraydeque_push_back() {
        let mut arr: ArrayDeque<i32, 3> = ArrayDeque::new();
        assert!(arr.push_back(1));
        assert!(arr.push_back(2));
        assert!(arr.push_back(3));
        assert!(!arr.push_back(4)); // ArrayDeque is full
        assert_eq!(arr.len(), 3);
        assert!(arr.is_full());
    }

    #[test]
    fn test_arraydeque_push_front() {
        let mut arr: ArrayDeque<i32, 3> = ArrayDeque::new();
        assert!(arr.push_front(1));
        assert!(arr.push_front(2));
        assert!(arr.push_front(3));
        assert!(!arr.push_front(4)); // ArrayDeque is full
        assert_eq!(arr.len(), 3);
        assert!(arr.is_full());
    }

    #[test]
    fn test_arraydeque_pop_back() {
        let mut arr: ArrayDeque<i32, 3> = ArrayDeque::new();
        arr.push_back(1);
        arr.push_back(2);
        arr.push_back(3);
        assert_eq!(arr.pop_back(), Some(3));
        assert_eq!(arr.pop_back(), Some(2));
        assert_eq!(arr.pop_back(), Some(1));
        assert_eq!(arr.pop_back(), None);
        assert!(arr.is_empty());
    }

    #[test]
    fn test_arraydeque_pop_front() {
        let mut arr: ArrayDeque<i32, 3> = ArrayDeque::new();
        arr.push_back(1);
        arr.push_back(2);
        arr.push_back(3);
        assert_eq!(arr.pop_front(), Some(1));
        assert_eq!(arr.pop_front(), Some(2));
        assert_eq!(arr.pop_front(), Some(3));
        assert_eq!(arr.pop_front(), None);
        assert!(arr.is_empty());
    }

    #[test]
    fn test_arraydeque_peek() {
        let mut arr: ArrayDeque<i32, 3> = ArrayDeque::new();
        assert_eq!(arr.back(), None);
        assert_eq!(arr.front(), None);
        arr.push_back(1);
        arr.push_back(2);
        arr.push_back(3);
        assert_eq!(arr.front(), Some(&1));
        assert_eq!(arr.pop_front(), Some(1));
        assert_eq!(arr.back(), Some(&3));
        assert_eq!(arr.pop_back(), Some(3));
        assert_eq!(arr.back(), Some(&2));
        assert_eq!(arr.pop_back(), Some(2));
        assert!(arr.is_empty());
    }

    #[test]
    fn test_arraydeque_mixed_operations() {
        let mut arr: ArrayDeque<i32, 3> = ArrayDeque::new();
        arr.push_back(1);
        arr.push_front(2);
        arr.push_back(3);
        assert_eq!(arr.len(), 3);
        assert_eq!(arr.pop_front(), Some(2));
        assert_eq!(arr.pop_back(), Some(3));
        assert_eq!(arr.pop_back(), Some(1));
        assert!(arr.is_empty());
    }

    #[test]
    fn test_arraydeque_wrap_around() {
        let mut arr: ArrayDeque<i32, 3> = ArrayDeque::new();
        for i in 0..30 {
            if arr.is_full() {
                assert_eq!(arr.pop_front(), Some(i - 3));
            }
            arr.push_back(i);
        }
        assert_eq!(arr.pop_front(), Some(30 - 3));
        assert_eq!(arr.pop_front(), Some(30 - 2));
        assert_eq!(arr.pop_front(), Some(30 - 1));
        assert!(arr.is_empty());
    }

    #[test]
    fn test_arraydeque_drop() {
        use std::{cell::RefCell, rc::Rc};

        let counter = Rc::new(RefCell::new(0));

        struct Dropable(Rc<RefCell<i32>>);

        impl Drop for Dropable {
            fn drop(&mut self) {
                *self.0.borrow_mut() += 1;
            }
        }

        {
            let mut arr: ArrayDeque<Dropable, 3> = ArrayDeque::new();
            arr.push_back(Dropable(Rc::clone(&counter)));
            arr.push_back(Dropable(Rc::clone(&counter)));
            arr.push_back(Dropable(Rc::clone(&counter)));
        } // arr goes out of scope here

        assert_eq!(*counter.borrow(), 3);

        // wrap around
        *counter.borrow_mut() = 0;
        {
            let mut arr: ArrayDeque<Dropable, 3> = ArrayDeque::new();
            arr.push_back(Dropable(Rc::clone(&counter)));
            arr.push_back(Dropable(Rc::clone(&counter)));
            arr.push_back(Dropable(Rc::clone(&counter)));
            arr.pop_front();
            arr.pop_front();
            arr.push_back(Dropable(Rc::clone(&counter)));
        } // arr goes out of scope here

        assert_eq!(*counter.borrow(), 4);
    }
}
