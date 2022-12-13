use std::cmp::Ordering;

#[derive(PartialEq, Eq, Debug, Clone)]
enum Packet
{
    Num(usize),
    List(Vec<Packet>),
}

fn parse(line: &str) -> (Option<Packet>, &str)
{
    let mut line = line;
    if line.starts_with('[')
    {
        let mut list = Vec::new();
        while !line.starts_with(']')
        {
            line = &line[1..];
            let (p, new_s) = parse(line);
            line = new_s;
            if let Some(p) = p
            {
                list.push(p);
            }
        }
        if line.len() > 1
        {
            line = &line[1..];
        }
        (Some(Packet::List(list)), line)
    }
    else
    {
        match line.find([',', ']'])
        {
            Some(f) if f > 0 =>
            {
                let p = line[..f].parse::<usize>().unwrap();
                line = &line[f..];
                (Some(Packet::Num(p)), line)
            },
            _ => (None, line),
        }
    }
}

impl PartialOrd for Packet
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>
    {
        Some(self.cmp(other))
    }
}

impl Ord for Packet
{
    fn cmp(&self, other: &Self) -> Ordering
    {
        match (self, other)
        {
            (Packet::Num(left), Packet::Num(right)) => left.cmp(right),
            (Packet::List(left), Packet::List(right)) => left.cmp(right),
            (Packet::List(left), Packet::Num(_)) => left.as_slice().cmp(&[other.clone()]),
            (Packet::Num(_), Packet::List(right)) => [self.clone()].as_slice().cmp(right),
        }
    }
}


fn task_one(input: &[String]) -> usize
{
    let mut count = 0;
    for (i, pairs) in input.split(|line| line.is_empty()).enumerate()
    {
        let p1 = parse(&pairs[0]).0;
        let p2 = parse(&pairs[1]).0;

        if p1.cmp(&p2) == Ordering::Less
        {
            count += i + 1;
        }
    }

    count
}

fn task_two(input: &[String]) -> usize
{
    let mut vec = input.iter().flat_map(|line| parse(line).0).collect::<Vec<_>>();

    let (Some(div1), _) = parse("[[2]]") else { panic!() };
    let (Some(div2), _) = parse("[[6]]") else { panic!() };

    vec.push(div1.clone());
    vec.push(div2.clone());

    vec.sort_unstable();

    [div1, div2]
        .into_iter()
        .map(|d| vec.iter().position(|packet| *packet == d).unwrap() + 1)
        .product()
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
