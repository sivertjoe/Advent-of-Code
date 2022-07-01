#[derive(Clone)]
struct Desc
{
    x: (i64, i64),
    y: (i64, i64),
    z: (i64, i64),
}

#[derive(Clone)]
struct Action
{
    turn_on: bool,
    desc:    Desc,
}

impl std::str::FromStr for Action
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let get_letter = |letter: char| {
            let l = s.find(letter).unwrap() + 2;
            let r = l + s[l..].find(',').unwrap_or(s.len() - l);

            let lr = l + s[l..].find('.').unwrap();

            let fst = s[l..lr].parse::<i64>().unwrap();
            let snd = s[lr + 2..r].parse::<i64>().unwrap();

            (fst, snd)
        };

        let desc = Desc {
            x: get_letter('x'), y: get_letter('y'), z: get_letter('z')
        };
        Ok(Self {
            turn_on: s.starts_with("on"),
            desc,
        })
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
    time(Task::One, task_one, input.clone());
    time(Task::Two, task_two, input);
}

fn task_one(input: Vec<Action>) -> i64
{
    let accept = |d: &Desc| {
        d.x.0.abs() <= 50
            && d.x.1.abs() <= 50
            && d.y.0.abs() <= 50
            && d.y.1.abs() <= 50
            && d.z.0.abs() <= 50
            && d.z.1.abs() <= 50
    };
    solve(input, accept)
}

fn task_two(input: Vec<Action>) -> i64
{
    solve(input, |_| true)
}

#[inline]
fn compare_axis<S, C>(curr: &Desc, cube: &mut Desc, s: S, c: C, vec: &mut Vec<Desc>)
where
    S: Fn(&Desc) -> (i64, i64),
    C: Fn(&mut Desc) -> &mut (i64, i64),
{
    if s(cube).0 < s(curr).0
    {
        let mut new = cube.clone();
        *c(&mut new) = (s(cube).0, s(curr).0 - 1);
        vec.push(new);
        c(cube).0 = s(curr).0;
    }

    if s(cube).1 > s(curr).1
    {
        let mut new = cube.clone();
        *c(&mut new) = (s(curr).1 + 1, s(cube).1);
        vec.push(new);
        c(cube).1 = s(curr).1;
    }
}

fn solve<A>(input: Vec<Action>, accept: A) -> i64
where
    A: Fn(&Desc) -> bool,
{
    /* Oh yeah, nice and readalbe. `for loop` pfff
     */
    input
        .into_iter()
        .fold(
            Vec::new(),
            |
                cuboids,
                Action {
                    desc,
                    turn_on,
                },
            | {
                let mut new = Vec::new();
                for mut cube in cuboids.into_iter().filter(&accept)
                {
                    if !inside(&cube, &desc)
                    {
                        new.push(cube);
                    }
                    else
                    {
                        /* In this function we check to see if `cube` overlaps with `desc`
                         * in any of the axis. If it does, we create a new cube, with the
                         * overlapping part removed, and constraint cube in that axis
                         */
                        compare_axis(&desc, &mut cube, |a| a.x, |a| &mut a.x, &mut new);
                        compare_axis(&desc, &mut cube, |a| a.y, |a| &mut a.y, &mut new);
                        compare_axis(&desc, &mut cube, |a| a.z, |a| &mut a.z, &mut new);
                    }
                }

                if turn_on && accept(&desc)
                {
                    new.push(desc);
                }
                new
            },
        )
        .into_iter()
        .map(calc)
        .sum::<i64>()
}

fn calc(d: Desc) -> i64
{
    (d.x.1 - d.x.0 + 1) * (d.y.1 - d.y.0 + 1) * (d.z.1 - d.z.0 + 1)
}

#[inline]
fn inside(a: &Desc, b: &Desc) -> bool
{
    use std::cmp::{max, min};
    max(a.x.0, b.x.0) <= min(a.x.1, b.x.1)
        && max(a.y.0, b.y.0) <= min(a.y.1, b.y.1)
        && max(a.z.0, b.z.0) <= min(a.z.1, b.z.1)
}
