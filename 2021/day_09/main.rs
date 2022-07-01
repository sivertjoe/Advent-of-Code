use std::collections::*;

struct Matrix
{
    vec: Vec<i32>,
    dim: (i32, i32),
}

impl std::ops::Index<[i32; 2]> for Matrix
{
    type Output = i32;

    fn index(&self, idx: [i32; 2]) -> &Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &self.vec[idx]
    }
}

impl std::str::FromStr for Matrix
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let vec: Vec<Vec<i32>> = s
            .lines()
            .map(|line| line.as_bytes().iter().map(|b| (b - b'0') as i32).collect())
            .collect();
        let dim = (vec[0].len() as i32, vec.len() as i32);

        let vec = vec.into_iter().flatten().collect::<Vec<_>>();
        Ok(Self {
            vec,
            dim,
        })
    }
}

fn read_input<P>(path: P) -> Matrix
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path).unwrap().parse().unwrap()
}

fn time<F, T, U>(pre: &'static str, f: F, arg: T)
where
    F: Fn(T) -> U,
    U: std::fmt::Display,
{
    let t = std::time::Instant::now();
    let res = f(arg);
    println!("({}ms) \tTask {}: {}", t.elapsed().as_millis(), pre, res);
}

fn main()
{
    let mat = read_input("input");
    time("one", task_one, &mat);
    time("two", task_two, &mat);
}

fn task_one(mat: &Matrix) -> i32
{
    (0..(mat.vec.len() as i32)).fold(0, |acc, i| {
        let x = i % mat.dim.0;
        let y = i / mat.dim.0;

        acc + (is_low_point(mat, x, y) as i32) * (mat[[x, y]] + 1)
    })
}

fn task_two(mat: &Matrix) -> i32
{
    let mut heap = (0..(mat.vec.len() as i32)).fold(BinaryHeap::new(), |mut acc, i| {
        let x = i % mat.dim.0;
        let y = i / mat.dim.0;

        if is_low_point(mat, x, y)
        {
            acc.push(1 + basin(mat, x, y, &mut HashSet::new()));
        }

        acc
    });

    /* Sadly, into_iter_sorted() is nightly only, thus
     * .into_iter_sorted().take(3).product() is not possible
     */
    heap.pop().unwrap() * heap.pop().unwrap() * heap.pop().unwrap()
}

#[inline]
fn within_bounds(mat: &Matrix, x: i32, y: i32) -> bool
{
    x >= 0 && x < mat.dim.0 && y >= 0 && y < mat.dim.1
}

#[inline]
fn is_low_point(mat: &Matrix, x: i32, y: i32) -> bool
{
    [(1, 0), (-1, 0), (0, 1), (0, -1)]
        .into_iter()
        .filter(|(dx, dy)| within_bounds(mat, x + dx, y + dy))
        .all(|(dx, dy)| mat[[x, y]] < mat[[x + dx, y + dy]])
}

fn basin(mat: &Matrix, x: i32, y: i32, seen: &mut HashSet<(i32, i32)>) -> i32
{
    [(0, 1), (0, -1), (1, 0), (-1, 0)]
        .into_iter()
        .filter_map(|(dx, dy)| {
            let dx = x + dx;
            let dy = y + dy;
            (within_bounds(mat, dx, dy)
                && mat[[dx, dy]] != 9
                && mat[[dx, dy]] > mat[[x, y]]
                && seen.insert((dx, dy)))
            .then(|| 1 + basin(mat, dx, dy, seen))
        })
        .sum()
}
