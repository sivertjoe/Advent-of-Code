fn produce<const N: usize>(vec: Vec<bool>) -> Vec<bool>
{
    let mut vec = vec;
    while vec.len() <= N
    {
        let mut a = vec;
        let b = a.clone().into_iter().map(|b| !b).rev();
        a.push(false);
        a.extend(b);
        vec = a;
    }
    vec.drain(N..);
    vec
}

fn checksum(vec: Vec<bool>) -> String
{
    let mut vec = vec;

    let next = |vec: Vec<bool>| vec.chunks(2).map(|arr| arr[0] == arr[1]).collect::<Vec<_>>();

    vec = next(vec);
    while vec.len() % 2 == 0
    {
        vec = next(vec);
    }
    vec.into_iter()
        .map(|b| {
            if b
            {
                '1'
            }
            else
            {
                '0'
            }
        })
        .collect()
}

fn solve<const N: usize>(input: &[String]) -> String
{
    let inp = input[0].chars().map(|ch| ch == '1').collect::<Vec<_>>();
    let dragon = produce::<N>(inp);
    checksum(dragon)
}

fn task_one(input: &[String]) -> String
{
    solve::<272>(input)
}

fn task_two(input: &[String]) -> String
{
    solve::<35651584>(input)
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
