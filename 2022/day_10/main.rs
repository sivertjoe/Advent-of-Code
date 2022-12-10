fn draw(s: &mut String, x: isize, cycles: isize)
{
    let xx = cycles % 40;

    if cycles % 40 == 0
    {
        s.push('\n');
    }
    if (-1..=1).map(|n| n + x).any(|x| x == xx)
    {
        s.push('#');
    }
    else
    {
        s.push('.');
    }
}

fn solve(input: &[String]) -> (isize, String)
{
    let mut add = None;
    let mut x = 1;

    let mut sum = 0;
    let mut crt = String::from("\n.");

    let mut iter = input.iter();

    for cycles in 1..
    {
        let n = (cycles + 20 - 1) / 40;
        if cycles == 40 * n + 20
        {
            sum += cycles * x;
        }

        if let Some(add) = add.take()
        {
            x += add;
        }
        else
        {
            match iter.next()
            {
                None => break,
                Some(s) if s == "noop" =>
                {},
                Some(addx) =>
                {
                    let (_, amount) = addx.split_once(' ').unwrap();
                    let amount = amount.parse::<isize>().unwrap();
                    add = Some(amount);
                },
            }
        }

        draw(&mut crt, x, cycles);
    }

    (sum, crt)
}

fn task_one(input: &[String]) -> isize
{
    solve(input).0
}

fn task_two(input: &[String]) -> String
{
    solve(input).1
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
