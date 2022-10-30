use std::collections::*;

fn parse_point(s: &str) -> Point<3, i32>
{
    // p=<-1021,-2406,1428>
    let vec: Vec<i32> = s[3..s.len() - 1].split(',').map(|token| token.parse().unwrap()).collect();
    point! {vec[0], vec[1], vec[2]}
}

struct Particle
{
    id:  usize,
    pos: Point<3, i32>,
    vel: Point<3, i32>,
    acc: Point<3, i32>,
}

fn parse_particle(line: &String) -> Particle
{
    let mut iter = line.split(", ").map(parse_point);
    Particle {
        id:  0,
        pos: iter.next().unwrap(),
        vel: iter.next().unwrap(),
        acc: iter.next().unwrap(),
    }
}

fn parse(input: &[String]) -> Vec<Particle>
{
    input.iter().map(parse_particle).collect()
}

fn task_one(input: &[String]) -> usize
{
    parse(input)
        .into_iter()
        .enumerate()
        .reduce(|acc, x| std::cmp::min_by_key(acc, x, |v| v.1.acc.manhattan()))
        .unwrap()
        .0
}

fn task_two(input: &[String]) -> usize
{
    let mut vec = parse(input);
    for (i, elem) in vec.iter_mut().enumerate()
    {
        elem.id = i;
    }
    loop
    {
        let mut map = HashMap::with_capacity(vec.len());
        for elem in &mut vec
        {
            elem.vel += elem.acc;
            elem.pos += elem.vel;
            map.entry(elem.pos).or_insert(Vec::new()).push(elem.id);
        }

        for p in map.into_values().filter(|v| v.len() > 1).flatten()
        {
            let pos = vec.iter().position(|elm| elm.id == p).unwrap();
            vec.swap_remove(pos);
        }

        vec.sort_by_key(|elem| elem.pos.manhattan());
        // I want to do: if vec.is_sorted_by_key(...)
        // But, this feature is unstable..! (1.64)
        if is_sorted_by_key(&vec, |elem| elem.acc.manhattan())
        {
            return vec.len();
        }
    }
}

fn is_sorted_by_key<T, U, F>(vec: &Vec<T>, f: F) -> bool
where
    F: Fn(&T) -> U,
    U: PartialEq + PartialOrd,
{
    for curr in 1..vec.len()
    {
        let prev = curr - 1;
        if f(&vec[curr]) < f(&vec[prev])
        {
            return false;
        }
    }
    true
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
    std::fs::read_to_string(path).unwrap().lines().map(String::from).collect()
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
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

impl Point<3, i32>
{
    fn manhattan(&self) -> i32
    {
        self.0.iter().map(|n| n.abs()).sum()
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
