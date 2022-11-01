fn solve<const N: usize>(
    input: &[String],
    keypad: &[[Option<char>; N]],
    x: usize,
    y: usize,
) -> String
{
    let mut y = y;
    let mut x = x;

    let mut code = String::new();
    for line in input
    {
        for ch in line.chars()
        {
            match ch
            {
                'U' =>
                {
                    if (0..N).contains(&(y - 1)) && keypad[y - 1][x].is_some()
                    {
                        y -= 1;
                    }
                },
                'D' =>
                {
                    if (0..N).contains(&(y + 1)) && keypad[y + 1][x].is_some()
                    {
                        y += 1;
                    }
                },
                'R' =>
                {
                    if (0..N).contains(&(x + 1)) && keypad[y][x + 1].is_some()
                    {
                        x += 1;
                    }
                },
                'L' =>
                {
                    if (0..N).contains(&(x - 1)) && keypad[y][x - 1].is_some()
                    {
                        x -= 1;
                    }
                },
                _ => unreachable!(),
            }
        }
        code.push(*keypad[y as usize][x as usize].as_ref().unwrap());
    }
    code
}

fn task_one(input: &[String]) -> String
{
    let vec = vec![[Some('1'), Some('2'), Some('3')], [Some('4'), Some('5'), Some('6')], [
        Some('7'),
        Some('8'),
        Some('9'),
    ]];

    solve(input, &vec, 1, 1)
}

fn task_two(input: &[String]) -> String
{
    let vec = vec![
        [None, None, Some('1'), None, None],
        [None, Some('2'), Some('3'), Some('4'), None],
        [Some('5'), Some('6'), Some('7'), Some('8'), Some('9')],
        [None, Some('A'), Some('B'), Some('C'), None],
        [None, None, Some('D'), None, None],
    ];
    solve(input, &vec, 0, 2)
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
