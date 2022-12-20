fn solve<const I: usize, const MUL: i64>(input: &[String]) -> i64
{
    let mut vec = input
        .iter()
        .enumerate()
        .map(|(i, n)| (i, MUL * n.parse::<i64>().unwrap()))
        .collect::<Vec<(usize, i64)>>();

    let vec2 = vec.clone();
    for _ in 0..I
    {
        for elem @ (_, num) in vec2.iter().copied()
        {
            let i = vec.iter().position(|n| *n == elem).unwrap();
            vec.remove(i);
            let new_pos = (i as i64 + num).rem_euclid(vec.len() as i64) as usize;
            vec.insert(new_pos, elem);
        }
    }
    let zero = vec.iter().position(|(_i, n)| *n == 0).unwrap();
    [1000, 2000, 3000]
        .into_iter()
        .map(|inc| vec[(zero + inc).rem_euclid(vec.len())].1)
        .sum()
}
fn task_one(input: &[String]) -> i64
{
    solve::<1, 1>(input)
}

fn task_two(input: &[String]) -> i64
{
    solve::<10, 811589153>(input)
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
