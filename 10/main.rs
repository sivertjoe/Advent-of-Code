use std::collections::*;

enum LineStatus
{
    Corrupted(char),
    Incomplete(VecDeque<char>),
}

impl std::str::FromStr for LineStatus
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let mut stack = VecDeque::new();
        for ch in s.chars()
        {
            match ch
            {
                '(' | '[' | '{' | '<' => stack.push_front(ch),
                _ => match stack.pop_front()
                {
                    Some(c) =>
                    {
                        if get_complement(ch) != c
                        {
                            return Ok(LineStatus::Corrupted(ch));
                        }
                    },
                    None =>
                    {
                        return Ok(LineStatus::Corrupted(ch));
                    },
                },
            }
        }
        Ok(LineStatus::Incomplete(stack))
    }
}

fn read_input<T, P>(path: P) -> Vec<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    std::io::BufReader::new(file)
        .lines()
        .flatten()
        .map(|line| line.parse::<T>().unwrap())
        .collect()
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
    let vec = read_input("input");
    time("one", task_one, &vec);
    time("two", task_two, &vec);
}

fn task_one(vec: &[String]) -> i32
{
    vec.iter()
        .filter_map(|line| match line.parse::<LineStatus>().unwrap()
        {
            LineStatus::Corrupted(ch) => Some(get_point(ch)),
            _ => None,
        })
        .sum()
}

fn task_two(vec: &[String]) -> i64
{
    let mut vec: Vec<_> = vec
        .iter()
        .filter_map(|line| match line.parse::<LineStatus>().unwrap()
        {
            LineStatus::Incomplete(stack) =>
            {
                Some(stack.into_iter().fold(0, |acc, ch| (acc * 5) + get_point2(ch)))
            },
            _ => None,
        })
        .collect();
    vec.sort_unstable();
    vec[vec.len() / 2]
}

fn get_point(ch: char) -> i32
{
    match ch
    {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => unreachable!(),
    }
}

fn get_complement(ch: char) -> char
{
    match ch
    {
        ')' => '(',
        ']' => '[',
        '}' => '{',
        '>' => '<',
        _ => unreachable!(),
    }
}

fn get_point2(ch: char) -> i64
{
    match ch
    {
        '(' => 1,
        '[' => 2,
        '{' => 3,
        '<' => 4,
        _ => unreachable!(),
    }
}
