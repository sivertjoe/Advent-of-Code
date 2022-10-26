#[derive(Debug)]
struct Point<const N: usize, T>([T; N]);

impl<const N: usize, T> std::ops::Index<usize> for Point<N, T>
{
    type Output = T;

    fn index(&self, idx: usize) -> &Self::Output
    {
        &self.0[idx]
    }
}

impl<const N: usize, T> std::ops::IndexMut<usize> for Point<N, T>
{
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output
    {
        &mut self.0[idx]
    }
}

impl<const N: usize, T> std::ops::Add for Point<N, T>
where
    T: std::ops::Add<Output = T> + Copy,
{
    type Output = Self;

    fn add(self, other: Self) -> Self
    {
        use std::mem::{self, MaybeUninit};

        let mut data: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
        for (i, elem) in data.iter_mut().enumerate()
        {
            *elem = MaybeUninit::new(self.0[i].add(other.0[i]));
        }

        Point(unsafe { mem::transmute_copy::<_, [T; N]>(&data) })
    }
}

impl<const N: usize, T> std::ops::AddAssign for Point<N, T>
where
    T: std::ops::Add<Output = T> + Copy,
{
    fn add_assign(&mut self, other: Self)
    {
        for i in 0..N
        {
            self[i] = self[i] + other[i];
        }
    }
}

#[macro_export]
macro_rules! point
{
    ( $($x: expr), +) =>
    {
        Point([
              $(($x),)+
        ])
    };
}
