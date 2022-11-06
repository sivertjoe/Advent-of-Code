fn parse(line: &str) -> Vec<bool>
{
    line.chars().map(|ch| ch == '.').collect()
}

fn is_trap(line: &[bool], index: usize, assumtion: bool) -> bool
{
    let mut idxs = [assumtion; 3];
    if index > 0
    {
        idxs[0] = line[index - 1];
    }
    idxs[1] = line[index];
    if index < line.len() - 1
    {
        idxs[2] = line[index + 1];
    }

    let is_trap = |idx: usize| !idxs[idx];
    !((is_trap(0) && is_trap(1) && !is_trap(2))
        || (!is_trap(0) && is_trap(1) && is_trap(2))
        || (is_trap(0) && !is_trap(1) && !is_trap(2))
        || (!is_trap(0) && !is_trap(1) && is_trap(2)))
}

fn solve<const N: usize>(input: &[String]) -> usize
{
    let mut vec = Vec::new();
    vec.push(parse(&input[0]));

    for step in 1..N
    {
        let new = (0..vec[step - 1].len()).map(|i| is_trap(&vec[step - 1], i, true)).collect();
        vec.push(new);
    }
    vec.into_iter().map(|line| line.into_iter().filter(|b| *b).count()).sum()
}

fn task_one(input: &[String]) -> usize
{
    solve::<40>(input)
}

fn task_two(input: &[String]) -> usize
{
    solve::<400000>(input)
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
