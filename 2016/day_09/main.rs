fn decompress(s: String) -> String
{
    let mut s = s.as_str();
    let mut new = String::new();
    while let Some(pos) = s.find('(')
    {
        new.push_str(&s[..pos]);
        let end = s.find(')').unwrap();

        let (fst, snd) = s[pos + 1..end].split_once('x').unwrap();
        let fst = fst.parse::<usize>().unwrap();
        let snd = snd.parse::<usize>().unwrap();

        let repeat = &s[end + 1..=end + fst];
        for _ in 0..snd
        {
            new.push_str(repeat);
        }
        s = &s[end + fst + 1..];
    }
    if !s.is_empty()
    {
        new.push_str(&s);
    }
    new
}

fn calc_len(s: &str) -> usize
{
    let mut s = s;
    let mut len = 0;

    while let Some(pos) = s.find('(')
    {
        len += s[..pos].len();

        let end = s.find(')').unwrap();

        let (fst, snd) = s[pos + 1..end].split_once('x').unwrap();
        let fst = fst.parse::<usize>().unwrap();
        let amount = snd.parse::<usize>().unwrap();

        let repeat = &s[end + 1..=end + fst];
        len += amount * calc_len(repeat);
        s = &s[end + fst + 1..];
    }

    if !s.is_empty()
    {
        len += s.len();
    }

    len
}

fn task_one(input: &[String]) -> usize
{
    decompress(input[0].clone()).len()
}


fn task_two(input: &[String]) -> usize
{
    calc_len(&input[0])
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
