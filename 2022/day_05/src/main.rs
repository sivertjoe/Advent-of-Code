fn solve(input: &[String], reverse: bool) -> String
{
    let pos = input.iter().position(|line| line.is_empty()).unwrap();
    let instructions = &input[pos + 1..];
    let input = &input[..pos];

    let mut vec: [Vec<char>; 9] = Default::default();
    for line in input
    {
        for (i, ch) in line
            .chars()
            .enumerate()
            .filter_map(|(i, ch)| ch.is_ascii_alphabetic().then_some((i / 4, ch)))
        {
            vec[i].insert(0, ch);
        }
    }

    for line in instructions
    {
        let elems = line.split_whitespace().flat_map(|tok| tok.parse()).collect::<Vec<usize>>();
        let &[fst, snd, thd] = elems.as_slice() else { panic!() }; // 👀
        let mut elems = vec[snd - 1].split_off(vec[snd - 1].len() - fst);
        if reverse
        {
            elems.reverse(); // task 2
        }
        vec[thd - 1].extend(elems);
    }

    vec.into_iter().map(|v| *v.last().unwrap()).collect()
}

fn task_one(input: &[String]) -> String
{
    solve(input, true)
}

fn task_two(input: &[String]) -> String
{
    solve(input, false)
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
