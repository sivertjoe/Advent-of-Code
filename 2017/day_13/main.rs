fn collide(vec: &Vec<(usize, usize)>, offset: usize) -> bool
{
    for (i, v) in vec
    {
        if (i + offset) % v == 0
        {
            return true;
        }
    }

    false
}

fn parse_line(line: &String) -> (usize, usize)
{
    let (a, b) = line.split_once(": ").unwrap();
    let a = a.parse::<usize>().unwrap();
    let b = b.parse::<usize>().unwrap();
    (a, (b - 1) * 2)
}

fn task_one(input: &[String]) -> usize
{
    let vec: Vec<(usize, usize)> = input.into_iter().map(parse_line).collect();

    let mut sum = 0;
    for (i, v) in vec
    {
        if i % v == 0
        {
            sum += i * (v / 2 + 1);
        }
    }

    sum
}

fn task_two(input: &[String]) -> usize
{
    let vec: Vec<(usize, usize)> = input.into_iter().map(parse_line).collect();

    for offset in 0..
    {
        if !collide(&vec, offset)
        {
            return offset;
        }
    }
    unreachable!()
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
