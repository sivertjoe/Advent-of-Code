use std::{cmp::Ordering, collections::*};
type MatItem = i32;

struct Matrix
{
    vec: Vec<MatItem>,
    dim: (i32, i32),
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

fn read_input<P>(path: P) -> Matrix
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    let vec: Vec<Vec<i32>> = std::io::BufReader::new(file)
        .lines()
        .flatten()
        .map(|line| line.as_bytes().iter().map(|b| (b - b'0') as i32).collect())
        .collect();

    let dim = (vec[0].len() as i32, vec.len() as i32);
    let vec = vec.into_iter().flatten().collect();

    Matrix {
        dim,
        vec,
    }
}

enum Task
{
    One,
    Two,
}

fn time<F, T, U>(task: Task, f: F, arg: T)
where
    F: Fn(T) -> U,
    U: std::fmt::Display,
{
    let t = std::time::Instant::now();
    let res = f(arg);
    let elapsed = t.elapsed().as_millis();

    match task
    {
        Task::One =>
        {
            println!("({}ms)\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        },
        Task::Two =>
        {
            println!("({}ms)\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        },
    };
}

fn main()
{
    let input = read_input("input");
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

/* Normal Dijkstra's. Code is based -- funnily enough -- on example code
 * from the binary_heap documentation:
 * https://doc.rust-lang.org/std/collections/binary_heap/index.html
 */
fn solve<F>(mat: &Matrix, dim: (i32, i32), f: F) -> i32
where
    F: Fn(i32, i32, &Matrix) -> i32,
{
    let mut dist = HashMap::new();
    dist.insert((0, 0), 0);

    let mut heap = BinaryHeap::new();

    heap.push(QItem {
        x: 0, y: 0, local_sum: 0, parent: (-1, -1)
    });

    while let Some(
        item @ QItem {
            x,
            y,
            local_sum,
            parent: _,
        },
    ) = heap.pop()
    {
        if (x, y) == (dim.0 - 1, dim.1 - 1)
        {
            return local_sum;
        }
        if local_sum > dist[&(x, y)]
        {
            continue;
        }

        for pair @ (x, y) in item.neighbors(dim)
        {
            let next = QItem {
                x,
                y,
                local_sum: local_sum + f(x, y, &mat),
                parent: (item.x, item.y),
            };

            let temp = dist.entry(pair).or_insert(i32::MAX);
            if next.local_sum < *temp
            {
                *temp = next.local_sum;
                heap.push(next);
            }
        }
    }
    unreachable!()
}

fn task_one(mat: &Matrix) -> i32
{
    let dim = (mat.dim.0, mat.dim.1);
    let f = |x: i32, y: i32, mat: &Matrix| mat[[x, y]];
    solve(mat, dim, f)
}

fn task_two(mat: &Matrix) -> i32
{
    let dim = (5 * mat.dim.0, 5 * mat.dim.1);
    let f = |x: i32, y: i32, mat: &Matrix| {
        let incx = x / mat.dim.0;
        let incy = y / mat.dim.1;

        let _x = x % mat.dim.0;
        let _y = y % mat.dim.1;


        ((mat[[_x, _y]] + incx + incy) - 1) % 9 + 1
    };

    solve(mat, dim, f)
}

#[derive(Eq, PartialEq)]
struct QItem
{
    local_sum: i32,
    x:         i32,
    y:         i32,
    parent:    (i32, i32),
}

impl QItem
{
    fn neighbors(&self, dim: (i32, i32)) -> impl Iterator<Item = (i32, i32)>
    {
        let x = self.x;
        let y = self.y;
        let parent = self.parent;

        [(1, 0), (-1, 0), (0, 1), (0, -1)].into_iter().filter_map(move |(dx, dy)| {
            ((0..dim.0).contains(&(x + dx))
                && (0..dim.1).contains(&(y + dy))
                .then(|| (x.clone() + dx, y.clone() + dy))
        })
    }
}

impl Ord for QItem
{
    fn cmp(&self, other: &Self) -> Ordering
    {
        other.local_sum.cmp(&self.local_sum)
    }
}

impl PartialOrd for QItem
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>
    {
        Some(self.cmp(other))
    }
}
