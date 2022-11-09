fn parse(line: &String) -> (usize, usize, usize)
{
    let mut iter = line.split('x');

    (
        iter.next().unwrap().parse().unwrap(),
        iter.next().unwrap().parse().unwrap(),
        iter.next().unwrap().parse().unwrap(),
    )
}

fn wrapping_paper((l, w, h): (usize, usize, usize)) -> usize
{
    let x = 2 * l * w;
    let y = 2 * w * h;
    let z = 2 * h * l;

    let min = std::cmp::min(x, std::cmp::min(y, z));
    x + y + z + min / 2
}

fn ribbon((l, w, h): (usize, usize, usize)) -> usize
{
    let arr = [l, w, h];
    let highest = *arr.iter().max().unwrap();
    let pos = arr.iter().position(|v| *v == highest).unwrap();

    let sum: usize = arr.iter().enumerate().filter_map(|(i, n)| (pos != i).then_some(2 * *n)).sum();

    let prod: usize = arr.iter().product();
    sum + prod
}

fn task_one(input: &[String]) -> usize
{
    input.iter().map(|line| wrapping_paper(parse(line))).sum()
}

fn task_two(input: &[String]) -> usize
{
    input.iter().map(|line| ribbon(parse(line))).sum()
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
