fn dir(s: &str) -> Point<3, i32>
{
    match s
    {
        "n" => point![1, -1, 0],
        "nw" => point![0, -1, 1],
        "ne" => point![1, 0 ,-1],
        "se" => point![0, 1, -1],
        "s" => point![-1, 1, 0],
        "sw" => point![-1, 0, 1],
        _ => unreachable!()

    }
}

fn dist(point: &Point<3, i32>) -> i32
{
    (point[0].abs() + point[1].abs() + point[2].abs()) / 2
}

fn solve(input: &[String]) -> (i32, i32)
{
    let mut max = 0;
    let mut point = Point([0, 0, 0]);
    for d in input[0].split(",")
    {
        point += dir(d);
        max = std::cmp::max(max, dist(&point));
    }
    (dist(&point), max)
}

fn task_one(input: &[String]) -> i32
{
    solve(input).0
}

fn task_two(input: &[String]) -> i32
{
    solve(input).1
}

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn read_input<P>(path: P) -> Vec<String>
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
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
where T: std::ops::Add<Output=T> + Copy
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
where T: std::ops::Add<Output=T> + Copy
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

