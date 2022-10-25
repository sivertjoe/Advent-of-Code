// Enum representing what to count
#[derive(Clone, Copy)]
enum Count
{
    Score,
    Garbage
}

fn parse(s: &str, score: i32, count: Count) -> Option<(&str, i32)>
{
    let ch = s.chars().next();
    if ch.is_none() { return None; }

    match s.chars().next().unwrap() 
    {
        '{' => Some(sum(&s[1..], if let Count::Garbage = count { 0 } else { score + 1 }, count)),
        '}' => None,
        ',' => Some((&s[1..], 0)),
        '<' => 
        {
            let mut sum = 0;
            let mut s = &s[1..];
            // SAFETY: a < should (eventually at least) be followed by a >
            let mut ch = s.chars().next().unwrap();
            while ch != '>'
            {
                let skip = ch == '!';
                if skip
                {
                    s = &s[2..];
                }
                else 
                {
                    sum += 1;
                    s = &s[1..];
                }
                ch = s.chars().next().unwrap();
            }
        

            if let Count::Score = count 
            {
                sum = 0;
            }
            if s.len() >= 1
            {
                Some((&s[1..], sum))
            }
            else 
            {
                Some((s, sum))
            }

        }
        _ => unreachable!()
    }
}

fn sum(s: &str, score: i32, count: Count) -> (&str, i32)
{
    let mut s = s;
    let mut sum = match count { Count::Score => score, _ => 0 };

    while let Some((new_s, inc_sum)) = parse(s, score, count)
    {
        s = new_s;
        sum += inc_sum;
    }
    if s.len() >= 1 
    {
        (&s[1..], sum)
    }
    else 
    {
        (s, sum)
    }
}

fn task_one(input: &[String]) -> i32
{
    sum(&input[0], 0, Count::Score).1
}

fn task_two(input: &[String]) -> i32
{
    sum(&input[0], 0, Count::Garbage).1
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
