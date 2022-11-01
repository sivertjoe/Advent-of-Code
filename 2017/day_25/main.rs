use std::collections::*;


#[derive(Debug)]
struct State
{
    write:  [u32; 2],
    r#move: [i32; 2],
    next:   [char; 2],
}

fn parse_block(input: &[String]) -> (u32, i32, char)
{
    let sym = |s: &String| s.split_whitespace().last().unwrap().chars().next().unwrap();

    let write = sym(&input[0]).to_digit(10).unwrap();
    let r#move = if sym(&input[1]) == 'r' { 1 } else { -1 };
    let next = sym(&input[2]);

    (write, r#move, next)
}

fn parse_state(input: &[String]) -> (char, State)
{
    let name = input[0].split_whitespace().last().unwrap().chars().next().unwrap();
    let b0 = parse_block(&input[2..]);
    let b1 = parse_block(&input[6..]);

    (name, State {
        write: [b0.0, b1.0], r#move: [b0.1, b1.1], next: [b0.2, b1.2]
    })
}

fn task_one(input: &[String]) -> usize
{
    let begin = input[0].split_whitespace().last().unwrap().chars().next().unwrap();
    let steps = input[1].split_whitespace().nth(5).unwrap().parse::<usize>().unwrap();

    let mut states = HashMap::new();
    for i in 0..input.len()
    {
        if input[i].len() == 0
        {
            let (name, state) = parse_state(&input[i + 1..]);
            states.insert(name, state);
        }
    }

    let mut tape = HashMap::new();
    let mut pos = 0;

    let mut st = begin;

    for _ in 0..steps
    {
        let curr = tape.entry(pos).or_insert(0);

        let new = states[&st].write[*curr];
        let next = states[&st].next[*curr];

        pos += states[&st].r#move[*curr];
        *curr = new as usize;
        st = next;
    }


    tape.values().sum()
}

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
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
