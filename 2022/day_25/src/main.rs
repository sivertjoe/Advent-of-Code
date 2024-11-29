fn from_base_5(num: &String) -> i64
{
    let from_char = |ch| match ch
    {
        '2' => 2,
        '1' => 1,
        '0' => 0,
        '-' => -1,
        '=' => -2,
        _ => unreachable!(),
    };

    num.chars()
        .rev()
        .enumerate()
        .fold(0_i64, |acc, (i, x)| acc + 5_i64.pow(i as u32) * from_char(x))
}

fn to_base_5(num: i64) -> String
{
    let mut num = num;

    let mut res = Vec::new();
    while num > 0
    {
        let n = (num % 5) as u32;

        let (ch, inc) = match n
        {
            0..=2 => (std::char::from_digit(n, 10).unwrap(), 0),
            3 => ('=', 2),
            4 => ('-', 1),
            _ => unreachable!(),
        };

        res.push(ch);

        num += inc;
        num /= 5;
    }

    res.reverse();
    res.into_iter().collect()
}

fn task_one(input: &[String]) -> String
{
    to_base_5(input.iter().map(from_base_5).sum())
}

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
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
