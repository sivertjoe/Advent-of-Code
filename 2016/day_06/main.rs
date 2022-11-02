fn max_letter(letters: &[u8; 26]) -> char
{
    let mut max = 0;
    let mut index = 0;

    for (i, ch) in letters.iter().enumerate()
    {
        if *ch > max
        {
            max = *ch;
            index = i;
        }
    }
    (index as u8 + b'a') as char
}

fn min_letter(letters: &[u8; 26]) -> char
{
    let mut max = 255;
    let mut index = 0;

    for (i, ch) in letters.iter().enumerate()
    {
        if *ch < max
        {
            max = *ch;
            index = i;
        }
    }
    (index as u8 + b'a') as char
}

fn find_letter<F>(v: &[char], f: &F) -> char
where
    F: Fn(&[u8; 26]) -> char,
{
    let mut arr = [0; 26];

    for ch in v
    {
        arr[(*ch as u8 - b'a') as usize] += 1;
    }

    f(&arr)
}

fn solve<F>(input: &[String], f: F) -> String
where
    F: Fn(&[u8; 26]) -> char,
{
    let mut vec: [Vec<char>; 8] = Default::default();

    for line in input
    {
        for (i, ch) in line.chars().enumerate()
        {
            vec[i].push(ch);
        }
    }
    vec.into_iter().map(|v| find_letter(&v, &f)).collect()
}

fn task_one(input: &[String]) -> String
{
    solve(input, max_letter)
}

fn task_two(input: &[String]) -> String
{
    solve(input, min_letter)
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
