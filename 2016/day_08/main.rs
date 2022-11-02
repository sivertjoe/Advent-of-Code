use std::collections::*;

enum Rotation
{
    Column,
    Row,
}

enum Operation
{
    Rect(usize, usize),
    Rotate(Rotation, usize, usize),
}

fn parse(line: &String) -> Operation
{
    if line.starts_with("rect")
    {
        let (_, f) = line.split_once(' ').unwrap();
        let (a, b) = f.split_once('x').unwrap();
        let a = a.parse::<usize>().unwrap();
        let b = b.parse::<usize>().unwrap();
        Operation::Rect(a, b)
    }
    else
    {
        let start = line.find('=').unwrap();
        let end = line.find(" by").unwrap();
        let num = line[start + 1..end].parse::<usize>().unwrap();

        let amount = line[end + 4..].parse::<usize>().unwrap();

        let rot = if line.starts_with("rotate row") { Rotation::Row } else { Rotation::Column };

        Operation::Rotate(rot, num, amount)
    }
}

fn pretty<const Y: usize, const X: usize>(arr: [[char; X]; Y]) -> String
{
    let mut builder = String::new();
    for y in 0..Y
    {
        builder.push('\n');
        for x in 0..X
        {
            builder.push(arr[y][x]);
        }
    }
    builder
}

fn solve<const Y: usize, const X: usize>(input: &[String]) -> [[char; X]; Y]
{
    let mut message: [[char; X]; Y] = [['.'; X]; Y];
    for op in input.into_iter().map(parse)
    {
        match op
        {
            Operation::Rect(a, b) =>
            {
                for y in 0..b
                {
                    for x in 0..a
                    {
                        message[y][x] = '#';
                    }
                }
            },
            Operation::Rotate(rot, x, amount) => match rot
            {
                Rotation::Column =>
                {
                    let mut new = ['.'; Y];
                    for y in 0..Y
                    {
                        let index = (y as i32 - amount as i32).rem_euclid(Y as i32) as usize;
                        new[y] = message[index][x];
                    }

                    for y in 0..Y
                    {
                        message[y][x] = new[y];
                    }
                },
                Rotation::Row =>
                {
                    let y = x;
                    let mut new = ['.'; X];
                    for x in 0..X
                    {
                        let index = (x as i32 - amount as i32).rem_euclid(X as i32) as usize;
                        new[x] = message[y][index];
                    }

                    for x in 0..X
                    {
                        message[y][x] = new[x];
                    }
                },
            },
        }
    }

    message
}

fn task_one(input: &[String]) -> usize
{
    solve::<6, 50>(input)
        .iter()
        .map(|arr| arr.iter().filter(|c| **c == '#').count())
        .sum()
}

fn task_two(input: &[String]) -> String
{
    pretty(solve::<6, 50>(input))
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
