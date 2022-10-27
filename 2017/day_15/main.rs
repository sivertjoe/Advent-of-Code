fn get_num(line: &str) -> u64
{
    line.split_whitespace().last().unwrap().parse().unwrap()
}

#[inline]
fn get_next(prev: u64, factor: u64) -> u64
{
    const DIVISOR: u64 = 2147483647;
    (prev * factor) % DIVISOR
}

fn task_one(input: &[String]) -> u64
{
    let (mut a, a_fact) = (get_num(&input[0]), 16807);
    let (mut b, b_fact) = (get_num(&input[1]), 48271);

    let mut counter = 0;
    for _ in 0..40_000_000
    {
        a = get_next(a, a_fact);
        b = get_next(b, b_fact);

        let fa = a as u16;
        let fb = b as u16;
        if fa == fb
        {
            counter += 1;
        }
    }
    counter
}

fn get_pair(prev: &mut u64, factor: u64, div: u64)
{
    loop
    {
        *prev = get_next(*prev, factor);
        if *prev % div == 0
        {
            return;
        }
    }
}

fn task_two(input: &[String]) -> u64
{
    let (mut a, a_fact) = (get_num(&input[0]), 16807);
    let (mut b, b_fact) = (get_num(&input[1]), 48271);

    let mut counter = 0;
    for _ in 0..5_000_000
    {
        get_pair(&mut a, a_fact, 4);
        get_pair(&mut b, b_fact, 8);

        let fa = a as u16;
        let fb = b as u16;
        if fa == fb
        {
            counter += 1;
        }
    }
    counter
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
