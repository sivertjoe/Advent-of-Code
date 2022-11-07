use std::collections::*;

#[derive(Debug)]
enum Dir
{
    Left,
    Right,
}

#[derive(Debug)]
enum Mode
{
    Position(usize, usize),
    Letter(char, char),
}

#[derive(Debug)]
enum RotateMode
{
    Direction(Dir, usize),
    Letter(char),
}

#[derive(Debug)]
enum Ins
{
    Swap(Mode),
    Rotate(RotateMode),
    Reverse(usize, usize),
    Move(usize, usize),
}

fn parse_line(line: &String) -> Ins
{
    if line.starts_with("swap position")
    {
        let mut iter = line.split_whitespace();
        let x = iter.nth(2).unwrap().parse().unwrap();
        let y = iter.nth(2).unwrap().parse().unwrap();
        Ins::Swap(Mode::Position(x, y))
    }
    else if line.starts_with("swap letter")
    {
        let mut iter = line.split_whitespace();
        let x = iter.nth(2).unwrap().chars().next().unwrap();
        let y = iter.nth(2).unwrap().chars().next().unwrap();
        Ins::Swap(Mode::Letter(x, y))
    }
    else if line.starts_with("rotate")
    {
        if line.starts_with("rotate based")
        {
            let mut iter = line.split_whitespace();
            let x = iter.nth(6).unwrap().chars().next().unwrap();
            Ins::Rotate(RotateMode::Letter(x))
        }
        else
        {
            let mut iter = line.split_whitespace();
            let dir = if iter.nth(1).unwrap() == "left" { Dir::Left } else { Dir::Right };
            let x = iter.nth(0).unwrap().parse().unwrap();
            Ins::Rotate(RotateMode::Direction(dir, x))
        }
    }
    else if line.starts_with("reverse position")
    {
        let mut iter = line.split_whitespace();
        let x = iter.nth(2).unwrap().parse().unwrap();
        let y = iter.nth(1).unwrap().parse().unwrap();
        Ins::Reverse(x, y)
    }
    else if line.starts_with("move position")
    {
        let mut iter = line.split_whitespace();
        let x = iter.nth(2).unwrap().parse().unwrap();
        let y = iter.nth(2).unwrap().parse().unwrap();
        Ins::Move(x, y)
    }
    else
    {
        unreachable!()
    }
}

fn rotate(vec: &mut Vec<char>, dir: Dir, step: usize)
{
    match dir
    {
        Dir::Left =>
        {
            for _ in 0..step
            {
                vec.rotate_left(1);
            }
        },
        Dir::Right =>
        {
            for _ in 0..step
            {
                vec.rotate_right(1);
            }
        },
    }
}
fn scramble(ins: &[Ins], s: Vec<char>) -> String
{
    let mut vec: Vec<_> = s;
    for instruction in ins
    {
        match instruction
        {
            Ins::Swap(Mode::Position(x, y)) => vec.swap(*x, *y),
            Ins::Swap(Mode::Letter(x, y)) =>
            {
                let x = vec.iter().position(|ch| *ch == *x).unwrap();
                let y = vec.iter().position(|ch| *ch == *y).unwrap();
                vec.swap(x, y);
            },
            Ins::Rotate(RotateMode::Direction(Dir::Left, val)) => rotate(&mut vec, Dir::Left, *val),
            Ins::Rotate(RotateMode::Direction(Dir::Right, val)) =>
            {
                rotate(&mut vec, Dir::Right, *val)
            },
            Ins::Rotate(RotateMode::Letter(x)) =>
            {
                let idx = vec.iter().position(|ch| *ch == *x).unwrap();
                let mut times = 1 + idx;
                if idx >= 4
                {
                    times += 1;
                }
                rotate(&mut vec, Dir::Right, times);
            },
            Ins::Reverse(x, y) =>
            {
                let iter: Vec<_> = vec[*x..=*y].iter().cloned().rev().enumerate().collect();
                for (i, val) in iter
                {
                    vec[*x + i] = val;
                }
            },
            Ins::Move(x, y) =>
            {
                let val = vec.remove(*x);
                vec.insert(*y, val);
            },
        }
    }
    vec.into_iter().collect()
}

fn unique_permutations<T: Clone>(items: Vec<T>) -> Vec<Vec<T>>
where
    T: Ord,
{
    if items.len() == 1
    {
        vec![items]
    }
    else
    {
        let mut output: Vec<Vec<T>> = vec![];
        let mut unique_items = items.clone();
        unique_items.sort();
        unique_items.dedup();
        for first in unique_items
        {
            let mut remaining_elements = items.clone();

            let index = remaining_elements.iter().position(|x| *x == first).unwrap();
            remaining_elements.remove(index);

            for mut permutation in unique_permutations(remaining_elements)
            {
                permutation.insert(0, first.clone());
                output.push(permutation);
            }
        }
        output
    }
}

fn task_one(input: &[String]) -> String
{
    let ins = input.iter().map(parse_line).collect::<Vec<_>>();
    scramble(&ins, "abcdefgh".chars().collect())
}

fn task_two(input: &[String]) -> String
{
    let ins = input.iter().map(parse_line).collect::<Vec<_>>();
    let target = "fbgdceah";
    // brr
    for perm in unique_permutations("abcdefgh".chars().collect())
    {
        if scramble(&ins, perm.clone()) == target
        {
            return perm.into_iter().collect::<String>();
        }
    }
    unreachable!()
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
