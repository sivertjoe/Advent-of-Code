use std::collections::*;

type MatItem = Option<Cucumber>;

#[derive(Clone)]
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

impl std::ops::IndexMut<[i32; 2]> for Matrix
{
    fn index_mut(&mut self, idx: [i32; 2]) -> &mut Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &mut self.vec[idx]
    }
}

#[derive(Clone, PartialEq)]
enum Cucumber
{
    South,
    East,
}

fn read_input<P>(path: P) -> Matrix
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    let vec: Vec<_> = std::io::BufReader::new(file)
        .lines()
        .flatten()
        .map(|line| {
            line.chars()
                .map(|ch| match ch
                {
                    'v' => Some(Cucumber::South),
                    '>' => Some(Cucumber::East),
                    _ => None,
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let dim = (vec[0].len() as i32, vec.len() as i32);
    Matrix {
        dim,
        vec: vec.into_iter().flatten().collect(),
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

fn get_input_file() -> String
{
    std::env::args().nth(1).unwrap_or_else(|| "input".to_string())
}

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, input);
}

fn task_one(mat: Matrix) -> i32
{
    let mut mat = mat;
    for i in 1..
    {
        let (m, count) = step(mat);
        if count == 0
        {
            return i;
        }

        mat = m;
    }
    unreachable!()
}

fn step(mat: Matrix) -> (Matrix, usize)
{
    let mut res = 0;
    let mut next = mat.clone();
    let next_x = |i: i32| (i + 1) % mat.dim.0;
    let next_y = |i: i32| (i + 1) % mat.dim.1;

    for y in 0..mat.dim.1
    {
        for x in 0..mat.dim.0
        {
            if let Some(Cucumber::East) = mat[[x, y]]
            {
                if mat[[next_x(x), y]].is_none()
                {
                    next[[next_x(x), y]] = Some(Cucumber::East);
                    next[[x, y]] = None;
                    res += 1;
                }
            }
        }
    }
    let mut next2 = next.clone();
    for y in 0..mat.dim.1
    {
        for x in 0..mat.dim.0
        {
            if let Some(Cucumber::South) = next[[x, y]]
            {
                if next[[x, next_y(y)]].is_none()
                {
                    next2[[x, next_y(y)]] = Some(Cucumber::South);
                    next2[[x, y]] = None;
                    res += 1;
                }
            }
        }
    }

    (next2, res)
}
