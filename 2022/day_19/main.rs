use std::collections::*;

#[derive(Debug, Default)]
struct Blueprint
{
    ore_cost:      u32,
    clay_cost:     u32,
    obsidian_cost: (u32, u32),
    geode_cost:    (u32, u32),
}

fn parse(input: &[String]) -> Vec<Blueprint>
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
    bs
}

fn solve<const T: u32>(bp: Blueprint) -> u32
{
    let ore_cost = bp.ore_cost;
    let clay_cost = bp.clay_cost;
    let (obsidian_ore_cost, obsidian_clay_cost) = bp.obsidian_cost;
    let (geode_ore_cost, geode_obsidian_cost) = bp.geode_cost;
    let mut best = 0;

    //             ore     clay    obs     geo    time
    let start = ([[0, 1], [0, 0], [0, 0], [0, 0]], T);
    let mut vec = VecDeque::new();
    vec.push_back(start);
    let mut seen = HashSet::new();

    const ORE: usize = 0;
    const CLAY: usize = 1;
    const OBSIDIAN: usize = 2;
    const GEODE: usize = 3;

    const AMOUNT: usize = 0;
    // Robots
    const R: usize = 1;

    while let Some((mut arr, t)) = vec.pop_front()
    {
        best = std::cmp::max(best, arr[GEODE][AMOUNT]);
        if t == 0
        {
            continue;
        }

        let core = [ore_cost, clay_cost, obsidian_ore_cost, geode_ore_cost]
            .into_iter()
            .max()
            .unwrap();

        for (ob, tt) in [(ORE, core), (CLAY, obsidian_clay_cost), (OBSIDIAN, geode_obsidian_cost)]
        {
            if arr[ob][R] >= tt
            {
                arr[ob][R] = tt;
            }
            let v = t * tt - arr[ob][R] * (t - 1);
            if arr[ob][AMOUNT] >= v
            {
                arr[ob][AMOUNT] = v;
            }
        }

        let state = (arr, t - 1);

        if !seen.insert(state)
        {
            continue;
        }

        let mut ns = state;
        for i in [ORE, CLAY, OBSIDIAN, GEODE].into_iter()
        {
            ns.0[i][AMOUNT] += ns.0[i][R];
        }

        vec.push_back(ns.clone());

        let new_states: [(usize, &[(usize, u32)]); 4] = [
            (ORE, &[(ORE, ore_cost)]),
            (CLAY, &[(ORE, clay_cost)]),
            (OBSIDIAN, &[(ORE, obsidian_ore_cost), (CLAY, obsidian_clay_cost)]),
            (GEODE, &[(ORE, geode_ore_cost), (OBSIDIAN, geode_obsidian_cost)]),
        ];

        for (mineral, cond) in new_states
        {
            if cond.iter().all(|(check, lim)| arr[*check][AMOUNT] >= *lim)
            {
                let mut ns = ns.clone();
                ns.0[mineral][R] += 1;
                for (check, lim) in cond
                {
                    ns.0[*check][AMOUNT] -= *lim;
                }
                vec.push_back(ns);
            }
        }
    }
    best
}

fn task_one(input: &[String]) -> u32
{
    parse(input)
        .into_iter()
        .enumerate()
        .map(|(id, b)| {
            let id = id as u32;
            (id + 1) * solve::<24>(b)
        })
        .sum()
}


fn task_two(input: &[String]) -> u32
{
    parse(input).into_iter().take(3).map(solve::<32>).product()
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
