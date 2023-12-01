trait Bfs {
    type Item;
    type Index;

    fn compare(&self, index1: &Self::Index, index2: &Self::Index) -> bool;
    fn is_goal(&self, index: &Self::Index) -> bool;
    fn neighbors(&self, curr: &Self::Index) -> impl Iterator<Item = Self::Index>;

    fn bfs(&self, start: Self::Index) -> usize
    where
        Self::Index: Sized + Clone + Eq + std::hash::Hash,
    {
        use std::collections::*;
        let mut vec = VecDeque::new();
        let mut seen = HashSet::new();

        vec.push_back((0, start.clone()));
        seen.insert(start);

        while let Some((cost, pos)) = vec.pop_front() {
            if self.is_goal(&pos) {
                return cost;
            }

            for neighbor in self.neighbors(&pos) {
                if self.compare(&pos, &neighbor) && seen.insert(neighbor.clone()) {
                    vec.push_back((cost + 1, neighbor));
                }
            }
        }
        unreachable!()
    }
}

/*
 * EXAMPLE IMPLEMENTATION
 * BASED ON 2022/day_12
 *
struct Task(Vec<Vec<u8>>, u8);
impl Bfs for Task
{
    type Item=u8;
    type Index=(usize, usize);

    fn is_goal(&self, idx: &Self::Index) -> bool { self.0[idx.1][idx.0] == self.1 }
    fn compare(&self, idx1: &Self::Index, idx2: &Self::Index) -> bool
    {
        self.0[idx1.1][idx1.0] - 1 <= self.0[idx2.1][idx2.0]
    }
    fn neighbors(&self, idx: &Self::Index) -> impl Iterator<Item=Self::Index>
    {
        let my = self.0.len() as isize;
        let mx = self.0[0].len() as isize;
        let x = idx.0 as isize;
        let y = idx.1 as isize;
        [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
            .into_iter()
            .filter(move |(x, y)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
            .map(|(x, y)| (x as usize, y as usize))
    }
}

fn main()
{
    let vec = Vec::new(); // Get your input here
    let start = (0, 0);
    let t = Task(vec, b'E');
    let answer = t.bfs(start);
}
*/
