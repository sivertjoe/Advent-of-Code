fn round(list: &mut [usize], input: &[usize], current_pos: &mut usize, skip_size: &mut usize)
{
    #[allow(non_snake_case)]
    let N = list.len();

    for length in input
    {
        let mut end = (*current_pos + length - 1) % N;
        let mut start = *current_pos;
        for _ in 0..length / 2
        {
            list.swap(start, end);

            end = (end as i32 - 1).rem_euclid(N as i32) as usize;
            start = (start + 1) % N;
        }
        *current_pos = (*current_pos + length + *skip_size) % N;
        *skip_size += 1;
    }
}

fn task_one(input: &[String]) -> usize
{
    let mut list: Vec<usize> = (0..256).collect();
    let input: Vec<usize> = input[0].split(',').map(|token| token.parse().unwrap()).collect();
    round(&mut list, &input, &mut 0, &mut 0);
    list[0] * list[1]
}

fn task_two(input: &[String]) -> String
{
    let mut list: Vec<usize> = (0..256).collect();
    let mut current_pos = 0;
    let mut skip_size = 0;

    let input = input[0].clone();
    let input: Vec<usize> = input
        .bytes()
        .map(|b| b as usize)
        .chain(vec![17, 31, 73, 47, 23].into_iter())
        .collect();

    for _ in 0..64
    {
        round(&mut list, &input, &mut current_pos, &mut skip_size);
    }


    list.chunks(16)
        .map(|range| range.iter().cloned().reduce(|acc, item| acc ^ item).unwrap())
        .fold(String::new(), |acc, item| format!("{}{:02x}", acc, item))
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
