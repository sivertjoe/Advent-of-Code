struct Matrix<T>
{
    vec: Vec<T>,
    dim: (usize, usize),
}

impl<T> Matrix<T>
where
    T: std::fmt::Display,
{
    #[allow(dead_code)]
    fn print(&self)
    {
        for i in 0..self.vec.len()
        {
            if i % self.dim.0 == 0
            {
                println!();
            }
            print!("{}", self.vec[i]);
        }
        println!();
    }
}


impl<T> std::ops::Index<[usize; 2]> for Matrix<T>
{
    type Output = T;

    fn index(&self, idx: [usize; 2]) -> &Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &self.vec[idx]
    }
}

impl<T> std::ops::IndexMut<[usize; 2]> for Matrix<T>
{
    fn index_mut(&mut self, idx: [usize; 2]) -> &mut Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &mut self.vec[idx]
    }
}
