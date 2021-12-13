type MatItem = i32;

struct Matrix
{
    vec: Vec<MatItem>,
    dim: (i32, i32),
}

impl Matrix
{
    fn print(&self)
    {
        for i in 0..self.vec.len()
        {
            if i as i32 % self.dim.0 == 0
            {
                println!("");
            }
            print!("{}", self.vec[i]);
        }
        println!("");
    }
}


impl std::ops::Index<[i32; 2]> for Matrix
{
    type Output = MatItem;

    fn index(&self, idx: [i32; 2]) -> &Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &self.vec[idx]
    }
}

impl std::ops::IndexMut<[i32; 2]> for Matrix
{
    fn index_mut(&mut self, idx: [i32; 2]) -> &mut Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &mut self.vec[idx]
    }
}
