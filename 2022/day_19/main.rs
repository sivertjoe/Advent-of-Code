use std::collections::*;

#[derive(Debug, Default)]
struct Blueprint
{
    ore_cost:      u32,
    clay_cost:     u32,
    obsidian_cost: (u32, u32),
    geode_cost:    (u32, u32),
}


fn task_one(input: &[String]) -> u32
{
    let mut bs = Vec::new();
    for line in input
    {
        let mut words = line.split_whitespace().collect::<Vec<_>>();

        let ore_cost = words[6].parse().unwrap();
        let clay_cost = words[12].parse::<u32>().unwrap();
        let obsidian_cost = (words[18].parse().unwrap(), words[21].parse().unwrap());
        let geode_cost =
            (words[27].parse().unwrap(), words[30].trim_end_matches('.').parse().unwrap());

        let blueprint = Blueprint {
            ore_cost,
            clay_cost,
            obsidian_cost,
            geode_cost,
        };

        bs.push(blueprint);
    }


    bs.into_iter()
        .enumerate()
        .map(|(id, b)| {
            let id = id as u32;
            let s = solve::<24>(
                b.ore_cost,
                b.clay_cost,
                b.obsidian_cost.0,
                b.obsidian_cost.1,
                b.geode_cost.0,
                b.geode_cost.1,
            );
            (id + 1) * s
        })
        .sum()
}

#[allow(non_snake_case)]
fn solve<const T: u32>(Co: u32, Cc: u32, Co1: u32, Co2: u32, Cg1: u32, Cg2: u32) -> u32
{
    let mut best = 0;

    //          ore clay obsidian geodes r1 r2 r3 r4 time
    let start = (0, 0, 0, 0, 1, 0, 0, 0, T);
    let mut vec = VecDeque::new();
    vec.push_back(start);
    let mut seen = HashSet::new();

    while let Some((mut o, mut c, mut ob, g, mut r1, mut r2, mut r3, r4, t)) = vec.pop_front()
    {
        best = std::cmp::max(best, g);
        if t == 0
        {
            continue;
        }

        let core = [Co, Cc, Co1, Cg1].into_iter().max().unwrap();
        if r1 >= core
        {
            r1 = core;
        }
        if r2 >= Co2
        {
            r2 = Co2;
        }
        if r3 >= Cg2
        {
            r3 = Cg2;
        }
        if o >= t * core - r1 * (t - 1)
        {
            o = t * core - r1 * (t - 1);
        }
        if c >= t * Co2 - r2 * (t - 1)
        {
            c = t * Co2 - r2 * (t - 1);
        }
        if ob >= t * Cg2 - r3 * (t - 1)
        {
            ob = t * Cg2 - r3 * (t - 1);
        }

        let state = (o, c, ob, g, r1, r2, r3, r4, t);

        if seen.contains(&state)
        {
            continue;
        }
        seen.insert(state);

        vec.push_back((o + r1, c + r2, ob + r3, g + r4, r1, r2, r3, r4, t - 1));
        if o >= Co
        {
            vec.push_back((o - Co + r1, c + r2, ob + r3, g + r4, r1 + 1, r2, r3, r4, t - 1));
        }
        if o >= Cc
        {
            vec.push_back((o - Cc + r1, c + r2, ob + r3, g + r4, r1, r2 + 1, r3, r4, t - 1));
        }
        if o >= Co1 && c >= Co2
        {
            vec.push_back((o - Co1 + r1, c - Co2 + r2, ob + r3, g + r4, r1, r2, r3 + 1, r4, t - 1));
        }
        if o >= Cg1 && ob >= Cg2
        {
            vec.push_back((o - Cg1 + r1, c + r2, ob - Cg2 + r3, g + r4, r1, r2, r3, r4 + 1, t - 1));
        }
    }
    best
}

fn task_two(input: &[String]) -> u32
{
    let mut bs = Vec::new();
    for line in input
    {
        let words = line.split_whitespace().collect::<Vec<_>>();

        let ore_cost = words[6].parse().unwrap();
        let clay_cost = words[12].parse::<u32>().unwrap();
        let obsidian_cost = (words[18].parse().unwrap(), words[21].parse().unwrap());
        let geode_cost =
            (words[27].parse().unwrap(), words[30].trim_end_matches('.').parse().unwrap());

        let blueprint = Blueprint {
            ore_cost,
            clay_cost,
            obsidian_cost,
            geode_cost,
        };

        bs.push(blueprint);
    }


    bs.into_iter()
        .take(3)
        .map(|b| {
            solve::<32>(
                b.ore_cost,
                b.clay_cost,
                b.obsidian_cost.0,
                b.obsidian_cost.1,
                b.geode_cost.0,
                b.geode_cost.1,
            )
        })
        .product()
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
