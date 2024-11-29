fn task_one(input: &[String]) -> usize
{
    input
        .iter()
        .map(|line| {
            let c1 = &line[line.len() / 2..];
            let c2 = &line[..line.len() / 2];

            c1.bytes()
                .find(|ch| c2.bytes().any(|ch2| *ch == ch2))
                .map(|ch| match ch
                {
                    b'a'..=b'z' => ch - b'a' + 1,
                    b'A'..=b'Z' => ch - b'A' + 1 + 26,
                    _ => unreachable!(),
                })
                .unwrap_or(0) as usize
        })
        .sum()
}

fn task_two(input: &[String]) -> usize
{
    input
        .chunks(3)
        .map(|w| {
            w[0].bytes()
                .find(|ch| w[1].bytes().any(|c| c == *ch) && w[2].bytes().any(|c| c == *ch))
                .map(|ch| match ch
                {
                    b'a'..=b'z' => ch - b'a' + 1,
                    b'A'..=b'Z' => ch - b'A' + 1 + 26,
                    _ => unreachable!(),
                })
                .unwrap_or(0) as usize
        })
        .sum()
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
