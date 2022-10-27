use std::collections::*;

enum Move
{
    Spin(usize),
    Exchange(usize, usize),
    Partner(char, char),
}

fn parse(s: &str) -> Move
{
    match s.chars().next().unwrap()
    {
        's' => Move::Spin(s[1..].parse().unwrap()),
        'x' =>
        {
            let (a, b) = s[1..].split_once('/').unwrap();
            let a: usize = a.parse().unwrap();
            let b: usize = b.parse().unwrap();

            Move::Exchange(a, b)
        },
        'p' =>
        {
            let (a, b) = s[1..].split_once('/').unwrap();
            let a = a.chars().next().unwrap();
            let b = b.chars().next().unwrap();

            Move::Partner(a, b)
        },
        _ => unreachable!(),
    }
}

fn dance(dancers: &mut Vec<char>, moves: &[Move])
{
    for r#move in moves
    {
        match r#move
        {
            Move::Exchange(a, b) => dancers.swap(*a, *b),
            Move::Partner(a, b) =>
            {
                let p1 = dancers.iter().position(|c| *c == *a).unwrap();
                let p2 = dancers.iter().position(|c| *c == *b).unwrap();
                dancers.swap(p1, p2);
            },

            // s3 abcde => cdeab
            Move::Spin(len) =>
            {
                let mut new = Vec::with_capacity(dancers.len());
                for idx in dancers.len() - len..dancers.len()
                {
                    new.push(dancers[idx]);
                }
                for idx in 0..dancers.len() - len
                {
                    new.push(dancers[idx]);
                }
                *dancers = new;
            },
        }
    }
}

fn task_one(input: &[String]) -> String
{
    let mut dancers: Vec<char> = ('a'..='p').collect();
    let moves: Vec<Move> = input[0].split(',').map(parse).collect();

    dance(&mut dancers, &moves);

    dancers.into_iter().collect()
}

fn task_two(input: &[String]) -> String
{
    let mut dancers: Vec<char> = ('a'..='p').collect();
    let moves: Vec<Move> = input[0].split(',').map(parse).collect();

    let mut set = HashSet::new();
    let mut vec = Vec::new();

    for i in 0..1_000_000_000
    {
        dance(&mut dancers, &moves);

        let s: String = dancers.iter().collect();
        if !set.insert(s.clone())
        {
            let start = vec.iter().position(|s: &String| &s == &s).unwrap();

            let v = &vec[start..];
            return v[(1_000_000_000 - (i + 1)) % v.len()].clone();
        }
        vec.push(s);
    }
    // This line is not reached; however, I suppose it could 🤷‍♀️
    dancers.into_iter().collect()
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
