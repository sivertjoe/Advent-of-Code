enum Motion
{
    Up(i32),
    Down(i32),
    Forward(i32),
}

use Motion::*;

impl std::str::FromStr for Motion
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let (motion, unit) = s.split_once(' ').unwrap();
        let unit: i32 = unit.parse().unwrap();
        Ok(match motion
        {
            "forward" => Motion::Forward(unit),
            "down" => Motion::Down(unit),
            "up" => Motion::Up(unit),
            _ => unreachable!(),
        })
    }
}

fn read_input<T, P>(path: P) -> Vec<T>
where
    P: AsRef<std::path::Path>,
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    std::io::BufReader::new(file)
        .lines()
        .flatten()
        .flat_map(|s| T::from_str(&s))
        .collect()
}

fn time<F, T, U>(pre: &'static str, f: F, arg: T)
where
    F: Fn(T) -> U,
    U: std::fmt::Display,
{
    let t0 = std::time::Instant::now();
    let res = f(arg);
    let t1 = std::time::Instant::now();
    println!("Task {}: {}\t({}ms)", pre, res, t1.duration_since(t0).as_millis());
}

fn main()
{
    let vec = read_input("input");
    time("one", task_one, &vec);
    time("two", task_two, &vec);
}

fn task_one(vec: &[Motion]) -> i32
{
    let mut h = 0;
    let mut depth = 0;

    for motion in vec
    {
        match motion
        {
            Up(u) => depth -= u,
            Down(u) => depth += u,
            Forward(u) => h += u,
        };
    }
    h * depth
}

fn task_two(vec: &[Motion]) -> i32
{
    let mut h = 0;
    let mut depth = 0;
    let mut aim = 0;

    for motion in vec
    {
        match motion
        {
            Up(u) => aim -= u,
            Down(u) => aim += u,
            Forward(u) =>
            {
                h += u;
                depth += aim * u;
            },
        };
    }
    h * depth
}
