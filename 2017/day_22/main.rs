use std::collections::*;

fn parse(input: &[String]) -> ((i32, i32), HashMap<(i32, i32), State>)
{
    let middle = (input[0].len() as i32 / 2, input.len() as i32 / 2);
    let map = input
        .iter()
        .enumerate()
        .map(|(y, line)| {
            line.chars().enumerate().map(move |(x, ch)| {
                ((x as i32, y as i32), if ch == '#' { State::Infected } else { State::Clean })
            })
        })
        .flatten()
        .collect();
    (middle, map)
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum State
{
    Infected,
    Clean,
    Weak,
    Flag,
}

#[allow(dead_code)]
#[repr(i32)]
#[derive(Clone, Copy)]
enum Dir
{
    Up = 0,
    Right,
    Down,
    Left,
}

fn forward(p: (i32, i32), dir: &Dir) -> (i32, i32)
{
    match dir
    {
        &Dir::Left => (p.0 - 1, p.1),
        &Dir::Right => (p.0 + 1, p.1),
        &Dir::Down => (p.0, p.1 + 1),
        &Dir::Up => (p.0, p.1 - 1),
    }
}

fn rev(dir: &Dir) -> Dir
{
    let dir = *dir as i32;
    let dir = (dir + 2).rem_euclid(4);
    // SAFETY:
    // 🙏
    unsafe { std::mem::transmute(dir) }
}

fn turn(dir: &Dir, turn_dir: Dir) -> Dir
{
    let dir = *dir as i32;
    let turn_dir = if let Dir::Left = turn_dir { -1 } else { 1 };
    let dir = (dir + turn_dir).rem_euclid(4);
    // SAFETY:
    // 🙏
    unsafe { std::mem::transmute(dir) }
}

fn task_one(input: &[String]) -> usize
{
    let f = |state: State, pos: (i32, i32), facing: Dir| match state
    {
        State::Infected =>
        {
            (0, State::Clean, forward(pos, &turn(&facing, Dir::Right)), turn(&facing, Dir::Right))
        },
        State::Clean =>
        {
            (1, State::Infected, forward(pos, &turn(&facing, Dir::Left)), turn(&facing, Dir::Left))
        },
        _ => unreachable!(),
    };

    solve::<10000>(input, f)
}

fn solve<const N: usize>(
    input: &[String],
    f: impl Fn(State, (i32, i32), Dir) -> (usize, State, (i32, i32), Dir),
) -> usize
{
    let (mut pos, mut map) = parse(input);
    let mut facing = Dir::Up;
    let mut count = 0;
    for _ in 0..N
    {
        let entry = map.entry(pos).or_insert(State::Clean);
        let (inc, state, new_pos, new_facing) = f(*entry, pos, facing);
        count += inc;
        *entry = state;
        pos = new_pos;
        facing = new_facing;
    }
    count
}

fn task_two(input: &[String]) -> usize
{
    let f = |state: State, pos: (i32, i32), facing: Dir| match state
    {
        State::Clean =>
        {
            (0, State::Weak, forward(pos, &turn(&facing, Dir::Left)), turn(&facing, Dir::Left))
        },
        State::Weak => (1, State::Infected, forward(pos, &facing), facing),
        State::Infected =>
        {
            (0, State::Flag, forward(pos, &turn(&facing, Dir::Right)), turn(&facing, Dir::Right))
        },
        State::Flag => (0, State::Clean, forward(pos, &rev(&facing)), rev(&facing)),
    };
    solve::<10000000>(input, f)
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

#[test]
fn turn_correct()
{
    let dir = Dir::Up;
    assert!(if let Dir::Left = turn(&dir, Dir::Left) { true } else { false });
    assert!(if let Dir::Right = turn(&dir, Dir::Right) { true } else { false });
}
