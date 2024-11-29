use std::collections::*;

type Pos = (isize, isize);
const DIRS: [(isize, isize); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

fn rotate(facing: Pos, by: isize) -> Pos
{
    match by.rem_euclid(4)
    {
        0 => facing,
        1 => (-facing.1, facing.0),
        2 => (-facing.0, -facing.1),
        3 => (facing.1, -facing.0),
        _ => unreachable!(),
    }
}

fn facing_angle_to_axes(facing: isize) -> Pos
{
    DIRS[facing as usize]
}

fn facing_rev(p: Pos) -> usize
{
    match p
    {
        (1, 0) => 0,
        (0, 1) => 1,
        (-1, 0) => 2,
        (0, -1) => 3,
        _ => unreachable!(),
    }
}

fn create_edges() -> Vec<(Vec<Pos>, Vec<Pos>, isize, isize)>
{
    const LEN: isize = 50;
    vec![
        (
            (0..LEN).map(|i| (i, LEN * 2)).collect(),
            (LEN..LEN * 2).map(|i| (LEN, i)).collect(),
            3,
            2,
        ),
        (
            (LEN * 2..LEN * 3).map(|i| (0, i)).collect(),
            (0..LEN).rev().map(|i| (LEN, i)).collect(),
            2,
            2,
        ),
        (
            (LEN * 3..LEN * 4).map(|i| (0, i)).collect(),
            (LEN..LEN * 2).map(|i| (i, 0)).collect(),
            2,
            3,
        ),
        (
            (0..LEN).map(|i| (i, LEN * 4 - 1)).collect(),
            (LEN * 2..LEN * 3).map(|i| (i, 0)).collect(),
            1,
            3,
        ),
        (
            (LEN * 3..LEN * 4).map(|i| (LEN - 1, i)).collect(),
            (LEN..LEN * 2).map(|i| (i, LEN * 3 - 1)).collect(),
            0,
            1,
        ),
        (
            (LEN * 2..LEN * 3).map(|i| (LEN * 2 - 1, i)).collect(),
            (0..LEN).rev().map(|i| (LEN * 3 - 1, i)).collect(),
            0,
            0,
        ),
        (
            (LEN..LEN * 2).map(|i| (LEN * 2 - 1, i)).collect(),
            (LEN * 2..LEN * 3).map(|i| (i, LEN - 1)).collect(),
            0,
            1,
        ),
    ]
}

fn task_one(input: &[String]) -> usize
{
    let next = |(x, y): Pos, facing: usize, map: &HashMap<(usize, usize), u8>| -> (Pos, usize) {
        if DIRS[facing].0 != 0
        {
            let nx = x.overflowing_add(DIRS[facing].0).0 as usize;
            if map.contains_key(&(nx, y as usize))
            {
                return ((nx as isize, y), facing);
            }
            let iter = map.keys().filter_map(|(dx, dy)| (*dy == y as usize).then_some(*dx));
            let nx = if DIRS[facing].0 == 1 { iter.min().unwrap() } else { iter.max().unwrap() };
            ((nx as isize, y), facing)
        }
        else
        {
            let ny = y.overflowing_add(DIRS[facing].1).0 as usize;
            if map.contains_key(&(x as usize, ny))
            {
                return ((x, ny as isize), facing);
            }
            let iter = map.keys().filter_map(|(dx, dy)| (*dx == x as usize).then_some(*dy));
            let ny = if DIRS[facing].1 == 1 { iter.min().unwrap() } else { iter.max().unwrap() };
            ((x, ny as isize), facing)
        }
    };
    solve(input, next)
}

fn task_two(input: &[String]) -> usize
{
    let edges = create_edges();
    let next = |pos: Pos, facing: usize, _map: &HashMap<(usize, usize), u8>| {
        let dir = DIRS[facing];
        let next_pos = (pos.0 + dir.0, pos.1 + dir.1);
        let next_facing = facing;

        let (pos, facing) = edges
            .iter()
            .find_map(|edge| {
                edge.0
                    .iter()
                    .position(|p| *p == pos)
                    .and_then(|i| {
                        (edge.2 as usize == facing).then_some((
                            edge.1[i],
                            rotate(facing_angle_to_axes(facing as isize), (edge.3 + 2) - edge.2),
                        ))
                    })
                    .or_else(|| {
                        edge.1.iter().position(|p| *p == pos).and_then(|i| {
                            (edge.3 == facing as isize).then_some((
                                edge.0[i],
                                rotate(
                                    facing_angle_to_axes(facing as isize),
                                    (edge.2 + 2) - edge.3,
                                ),
                            ))
                        })
                    })
            })
            .unwrap_or((next_pos, facing_angle_to_axes(next_facing as isize)));

        (pos, facing_rev(facing))
    };
    solve(input, next)
}

fn parse(input: &[String]) -> HashMap<(usize, usize), u8>
{
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate()
    {
        for (x, b) in line.as_bytes().iter().enumerate()
        {
            if *b == b'#' || *b == b'.'
            {
                map.insert((x, y), *b);
            }
        }
    }
    map
}

fn get_path(line: &str) -> Vec<(isize, Option<isize>)>
{
    let mut s = line;
    let mut path = Vec::new();
    loop
    {
        if let Some(idx) = s.find(['L', 'R'])
        {
            let num = s[..idx].parse::<isize>().unwrap();
            let rotation = if &s[idx..=idx] == "L" { -1 } else { 1 };
            path.push((num, Some(rotation)));
            s = &s[idx + 1..];
        }
        else
        {
            let num = s[..].parse::<isize>().unwrap();
            path.push((num, None));
            break;
        }
    }
    path
}

fn solve<Next>(input: &[String], next: Next) -> usize
where
    Next: Fn(Pos, usize, &HashMap<(usize, usize), u8>) -> (Pos, usize),
{
    let idx = input.iter().position(|line| line.is_empty()).unwrap();

    let cube = &input[..idx];
    let line = &input[idx + 1];

    let map = parse(cube);
    let path = get_path(line);

    let mut pos = (cube[0].bytes().position(|b| b == b'.').unwrap() as isize, 0);
    let mut facing = 0;

    for (steps, rot) in path
    {
        for _ in 0..steps
        {
            let (next_pos, next_facing) = next(pos, facing, &map);
            if *map.get(&(next_pos.0 as usize, next_pos.1 as usize)).unwrap() == b'#'
            {
                break;
            }
            pos = next_pos;
            facing = next_facing;
        }
        if let Some(rot) = rot
        {
            facing = (facing as isize + rot).rem_euclid(4) as usize;
        }
    }
    (1000 * (pos.1 + 1) + 4 * (pos.0 + 1) + facing as isize) as usize
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
