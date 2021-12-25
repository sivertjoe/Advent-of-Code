use std::collections::*;

use Instruction::*;
use Number::*;
use Type::*;

enum Number
{
    Literal(i64),
    Register(char),
}

impl std::str::FromStr for Number
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        Ok(
            if let Ok(num) = s.parse::<i64>()
            {
                Literal(num)
            }
            else
            {
                Register(s.chars().next().unwrap())
            },
        )
    }
}

enum Instruction
{
    Inp(char),
    Add((char, Number)),
    Mul((char, Number)),
    Div((char, Number)),
    Mod((char, Number)),
    Eql((char, Number)),
}


impl std::str::FromStr for Instruction
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let mut split = s.split(' ');

        let inst = split.next().unwrap();
        let a: char = split.next().unwrap().chars().next().unwrap();

        if inst == "inp"
        {
            Ok(Inp(a))
        }
        else
        {
            let b: Number = split.next().unwrap().parse().unwrap();
            Ok(match inst
            {
                "inp" => Inp(a),
                "add" => Add((a, b)),
                "mul" => Mul((a, b)),
                "div" => Div((a, b)),
                "mod" => Mod((a, b)),
                "eql" => Eql((a, b)),
                _ => unreachable!(),
            })
        }
    }
}


fn read_input<T, P>(path: P) -> Vec<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    std::io::BufReader::new(file)
        .lines()
        .flatten()
        .map(|line| line.parse::<T>().unwrap())
        .collect()
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

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

enum Type
{
    Times(i64),
    Divide(i64),
}

fn solve(input: &[Instruction], task: Task) -> usize
{
    /* @Note:
     * These are _hard coded_ and unique for _my_ puzzle input.
     * To add your own, for each inp: if you see add x <num> where num > 9, then
     * you look further down and find add y <num> and add Times(<num>).
     * If add x <num> where < 10, then you add a Divide(<num>).
     */
    let c = vec![
        Times(6),
        Times(12),
        Times(5),
        Times(10),
        Divide(-16),
        Times(0),
        Times(4),
        Divide(-4),
        Times(14),
        Divide(-7),
        Divide(-8),
        Divide(-4),
        Divide(-15),
        Divide(-8),
    ];

    _solve(input, 0, &c, 0, &mut VecDeque::new(), &task).unwrap()
}

fn task_one(input: &[Instruction]) -> usize
{
    solve(input, Task::One)
}

fn task_two(input: &[Instruction]) -> usize
{
    solve(input, Task::Two)
}

fn _solve(
    input: &[Instruction],
    z: i64,
    number: &[Type],
    cursor: usize,
    vec: &mut VecDeque<i64>,
    task: &Task,
) -> Option<usize>
{
    match number.get(cursor)
    {
        Some(Times(n)) =>
        {
            let iter: Box<dyn Iterator<Item = i64>> = match *task
            {
                Task::One => Box::new((0..=9).rev()),
                Task::Two => Box::new(0..=9),
            };

            for w in iter
            {
                vec.push_back(w);
                let z = 26 * z + w + n;

                if let Some(res) = _solve(input, z, number, cursor + 1, vec, task)
                {
                    return Some(res);
                }

                let _ = vec.pop_back();
            }
            None
        },
        Some(Divide(d)) =>
        {
            let _z = z % 26 + d;
            if (1..=9).contains(&_z)
            {
                vec.push_back(_z);
                let res = _solve(input, z / 26, number, cursor + 1, vec, task);
                if res.is_none()
                {
                    let _ = vec.pop_back();
                }
                res
            }
            else
            {
                None
            }
        },

        None =>
        {
            if verify(input, vec.iter().copied())
            {
                let mut i = 1;
                Some(vec.iter().rev().fold(0_usize, |acc, x| {
                    let t = acc + (*x as usize) * i;
                    i *= 10;
                    t
                }))
            }
            else
            {
                None
            }
        },
    }
}

fn verify<I>(input: &[Instruction], mut num: I) -> bool
where
    I: Iterator<Item = i64>,
{
    let mut mem = HashMap::new();
    for ins in input
    {
        execute_instruction(ins, &mut mem, &mut num);
    }
    *mem.get(&'z').unwrap_or(&-1) == 0
}

#[inline]
fn exec<F>(mem: &mut HashMap<char, i64>, a: char, b: &Number, f: F)
where
    F: Fn(i64, i64) -> i64,
{
    let b: i64 = match b
    {
        Literal(n) => *n,
        Register(reg) => *mem.entry(*reg).or_insert(0),
    };

    let a = mem.entry(a).or_insert(0);
    *a = f(*a, b);
}

fn execute_instruction<I>(inst: &Instruction, mem: &mut HashMap<char, i64>, model: &mut I)
where
    I: Iterator<Item = i64>,
{
    match inst
    {
        Inp(c) =>
        {
            mem.insert(*c, model.next().unwrap());
        },
        Add((a, b)) =>
        {
            exec(mem, *a, b, |a, b| a + b);
        },
        Mul((a, b)) =>
        {
            exec(mem, *a, b, |a, b| a * b);
        },
        Div((a, b)) =>
        {
            exec(mem, *a, b, |a, b| a / b);
        },
        Mod((a, b)) =>
        {
            exec(mem, *a, b, |a, b| a % b);
        },
        Eql((a, b)) =>
        {
            exec(mem, *a, b, |a, b| (a == b) as i64);
        },
    }
}
