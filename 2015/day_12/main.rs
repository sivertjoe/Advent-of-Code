use std::collections::*;

#[derive(Debug)]
enum Value
{
    Number(i32),
    Text(String),
    Object(HashMap<String, Value>),
    Array(Vec<Value>),
}

fn parse(s: &str) -> (Value, &str)
{
    let mut s = s;
    if s.starts_with('[')
    {
        let mut vec = Vec::new();
        while !s.starts_with(']')
        {
            s = &s[1..];
            let (val, new_s) = parse(s);
            s = new_s;
            vec.push(val);
        }
        if !s.is_empty()
        {
            s = &s[1..];
        }
        (Value::Array(vec), s)
    }
    else if s.starts_with('{')
    {
        let mut map = HashMap::new();
        while !s.starts_with('}')
        {
            s = &s[1..];

            let key_start = s.find('"').unwrap();
            let key_end = s[key_start + 1..].find('"').unwrap();

            let key = s[key_start + 1..=key_end].to_string();

            let sep = s.find(':').unwrap();
            let (val, new_s) = parse(&s[sep + 1..]);
            s = new_s;
            map.insert(key, val);
        }
        if !s.is_empty()
        {
            s = &s[1..];
        }
        (Value::Object(map), s)
    }
    else
    {
        let c = s.find([',', ']', '}']).unwrap();
        (
            match s[..c].parse()
            {
                Ok(val) => Value::Number(val),
                _ => Value::Text(s[..c].trim_matches('"').to_owned()),
            },
            &s[c..],
        )
    }
}

fn sum(val: Value, part_one: bool) -> i32
{
    let mut res = 0;
    match val
    {
        Value::Number(num) => res += num,
        Value::Array(vec) =>
        {
            for v in vec
            {
                res += sum(v, part_one);
            }
        },
        Value::Object(map) =>
        {
            let mut local = 0;
            let mut flag = true;
            for (k, v) in map
            {
                if k == "red" || matches!(&v, &Value::Text(ref text) if text == "red")
                {
                    flag = false;
                }
                local += sum(v, part_one);
            }
            if flag || part_one
            {
                res += local;
            }
        },
        Value::Text(_) =>
        {}, // Text cannot be summed
    }
    res
}

fn task_one(input: &[String]) -> i32
{
    let part_one = true;
    let (val, _) = parse(&input[0]);
    sum(val, part_one)
}

fn task_two(input: &[String]) -> i32
{
    let (val, _) = parse(&input[0]);
    sum(val, false)
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
