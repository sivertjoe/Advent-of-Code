use std::collections::*;

fn update_head(head: (isize, isize), dir: &str) -> (isize, isize)
{
    if dir == "R"
    {
        (head.0 + 1, head.1)
    }
    else if dir == "L"
    {
        (head.0 - 1, head.1)
    }
    else if dir == "U"
    {
        (head.0, head.1 - 1)
    }
    else
    {
        (head.0, head.1 + 1)
    }
}

fn update_tail(head: &(isize, isize), tail: &(isize, isize)) -> (isize, isize)
{
    let xdiff = head.0 - tail.0;
    let ydiff = head.1 - tail.1;

    let one_axiss_diff = (xdiff == 0 && ydiff.abs() == 2) || (xdiff.abs() == 2 && ydiff == 0);
    let both_axis_diff =
        head != tail && head.0 != tail.0 && head.1 != tail.1 && (xdiff.abs() + ydiff.abs()) > 2;
    if one_axiss_diff || both_axis_diff
    {
        (tail.0 + xdiff.signum(), tail.1 + ydiff.signum())
    }
    else
    {
        *tail
    }
}

fn solve<const N: usize>(input: &[String]) -> usize
{
    let mut seen = HashSet::new();
    seen.insert((0, 0));

    let mut rope = [(0, 0); N];

    for line in input
    {
        let (dir, amount) = line.split_once(' ').unwrap();
        let amount = amount.parse::<usize>().unwrap();

        for _ in 0..amount
        {
            rope[0] = update_head(rope[0], dir);
            for i in 1..rope.len()
            {
                rope[i] = update_tail(&rope[i - 1], &rope[i]);
            }

            let tail = rope[N - 1];
            seen.insert(tail);
        }
    }

    seen.len()
}

fn task_one(input: &[String]) -> usize
{
    solve::<2>(input)
}

fn task_two(input: &[String]) -> usize
{
    solve::<10>(input)
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
