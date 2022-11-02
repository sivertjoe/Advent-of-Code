use std::collections::*;

fn abba(s: &[u8]) -> bool
{
    s[0] == s[3] && s[1] == s[2] && s[0] != s[1]
}

fn parse<'a>(line: &'a str) -> (Vec<&'a str>, Vec<&'a str>)
{
    let mut line = line;

    let mut first = Vec::new();
    let mut second = Vec::new();

    while let Some(pos) = line.find('[')
    {
        let end = line.find(']').unwrap();
        first.push(&line[0..pos]);
        second.push(&line[pos + 1..end]);
        line = &line[end + 1..];
    }
    if !line.is_empty()
    {
        first.push(&line);
    }
    (first, second)
}

fn supports_tls((first, second): (Vec<&str>, Vec<&str>)) -> bool
{
    let contains_abba =
        |v: Vec<&str>| v.into_iter().any(|token| token.as_bytes().windows(4).any(|b| abba(&b)));

    contains_abba(first) && !contains_abba(second)
}

fn supports_ssl((first, second): (Vec<&str>, Vec<&str>)) -> bool
{
    let bab =
        |s: &[u8], aba: &[u8]| s[0] == aba[1] && s[0] == s[2] && s[0] != s[1] && s[1] == aba[0];

    first
        .into_iter()
        .map(|s| s.as_bytes().windows(3).filter(|bs| bs[0] == bs[2] && bs[0] != bs[1]))
        .flatten()
        .any(|aba| second.iter().any(|s| s.as_bytes().windows(3).any(|b| bab(b, aba))))
}


fn task_one(input: &[String]) -> usize
{
    input.iter().filter(|s| supports_tls(parse(s))).count()
}

fn task_two(input: &[String]) -> usize
{
    input.iter().filter(|s| supports_ssl(parse(s))).count()
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
