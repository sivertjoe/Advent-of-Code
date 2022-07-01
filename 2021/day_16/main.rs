enum Packet
{
    Literal
    {
        version: i64, number: i64
    },
    Operator
    {
        version: i64, type_id: i64, sub: Vec<Packet>
    },
}


fn read_input<P>(path: P) -> String
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    std::io::BufReader::new(file).lines().next().unwrap().unwrap()
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

fn task_one(input: &str) -> i64
{
    let (p, _) = parse(&binary(input));
    version_sum(p)
}

fn task_two(input: &str) -> i64
{
    let (p, _) = parse(&binary(input));
    parse_tree(p)
}

fn parse_literal(s: &str) -> (Packet, &str)
{
    let version = i64::from_str_radix(&s[0..=2], 2).unwrap();

    let mut idx = 6;
    let mut number = String::new();

    loop
    {
        let bit = &s[idx..=idx];
        number.push_str(&s[idx + 1..idx + 5]);
        idx += 5;
        if bit == "0"
        {
            break;
        }
    }

    let number = i64::from_str_radix(&number, 2).unwrap();

    (
        Packet::Literal {
            version,
            number,
        },
        &s[idx..],
    )
}

fn parse_operator(s: &str, type_id: i64) -> (Packet, &str)
{
    let num = |s: &str| usize::from_str_radix(s, 2).unwrap();

    let version = i64::from_str_radix(&s[0..=2], 2).unwrap();

    let mut s = s;
    let length_type_id = 7;
    let (sub, s) = if &s[6..=6] == "0"
    {
        let bits_to_read = 15;
        let number_of_bits = num(&s[length_type_id..length_type_id + bits_to_read]);

        s = &s[length_type_id + bits_to_read..];

        let mut sub = Vec::new();
        let stop_length = s.len() - number_of_bits;
        while s.len() != stop_length
        {
            let (p, _s) = parse(s);
            sub.push(p);
            s = _s;
        }
        (sub, s)
    }
    else
    {
        let bits_to_read = 11;
        let sub_packets = num(&s[length_type_id..length_type_id + bits_to_read]);

        s = &s[length_type_id + bits_to_read..];

        (0..sub_packets).fold((Vec::new(), s), |(mut sub, s), _| {
            let (p, _s) = parse(s);
            sub.push(p);
            (sub, _s)
        })
    };
    (
        Packet::Operator {
            version,
            sub,
            type_id,
        },
        s,
    )
}

fn parse(s: &str) -> (Packet, &str)
{
    let type_id = i64::from_str_radix(&s[3..=5], 2).unwrap();

    match type_id
    {
        4 => parse_literal(s),
        _ => parse_operator(s, type_id),
    }
}

fn version_sum(p: Packet) -> i64
{
    match p
    {
        Packet::Literal {
            version,
            number: _,
        } => version,
        Packet::Operator {
            version,
            sub,
            type_id: _,
        } => version + sub.into_iter().map(version_sum).sum::<i64>(),
    }
}

#[inline]
fn compare_function<F>(sub: Vec<Packet>, f: F) -> i64
where
    F: Fn(i64, i64) -> bool,
{
    let mut iter = sub.into_iter();
    let first = iter.next().unwrap();
    let second = iter.next().unwrap();

    f(parse_tree(first), parse_tree(second)) as i64
}

fn parse_tree(p: Packet) -> i64
{
    match p
    {
        Packet::Literal {
            version: _,
            number,
        } => number,

        Packet::Operator {
            version: _,
            sub,
            type_id,
        } => match type_id
        {
            0 => sub.into_iter().map(parse_tree).sum(),
            1 => sub.into_iter().map(parse_tree).product(),
            2 => sub.into_iter().map(parse_tree).min().unwrap(),
            3 => sub.into_iter().map(parse_tree).max().unwrap(),

            5 => compare_function(sub, |a, b| a > b),
            6 => compare_function(sub, |a, b| a < b),
            7 => compare_function(sub, |a, b| a == b),
            _ => unreachable!(),
        },
    }
}

fn binary(hex: &str) -> String
{
    hex.chars().map(to_binary).collect()
}

fn to_binary(c: char) -> &'static str
{
    match c
    {
        '0' => "0000",
        '1' => "0001",
        '2' => "0010",
        '3' => "0011",
        '4' => "0100",
        '5' => "0101",
        '6' => "0110",
        '7' => "0111",
        '8' => "1000",
        '9' => "1001",
        'A' => "1010",
        'B' => "1011",
        'C' => "1100",
        'D' => "1101",
        'E' => "1110",
        'F' => "1111",
        _ => unreachable!(),
    }
}
