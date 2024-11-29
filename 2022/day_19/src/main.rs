type Point = (u32, u32, u32);

fn checked_sub(r1: Point, r2: Point) -> Option<Point>
{
    Some((r1.0.checked_sub(r2.0)?, r1.1.checked_sub(r2.1)?, r1.2.checked_sub(r2.2)?))
}

#[derive(Debug, Clone, Copy)]
struct Blueprint
{
    id:                  u32,
    ore_robot_cost:      Point,
    clay_robot_cost:     Point,
    obsidian_robot_cost: Point,
    geode_robot_cost:    Point,
}

fn blueprint3(line: &str, id: u32) -> Blueprint
{
    let words = line.split_whitespace().collect::<Vec<_>>();

    let ore_cost = words[6].parse().unwrap();
    let ore_robot_cost = (ore_cost, 0, 0);

    let clay_cost = words[12].parse().unwrap();
    let clay_robot_cost = (clay_cost, 0, 0);


    let obsidian_cost = (words[18].parse().unwrap(), words[21].parse().unwrap());
    let obsidian_robot_cost = (obsidian_cost.0, obsidian_cost.1, 0);

    let geode_cost = (words[27].parse().unwrap(), words[30].trim_end_matches('.').parse().unwrap());
    let geode_robot_cost = (geode_cost.0, 0, geode_cost.1);

    Blueprint {
        id,
        ore_robot_cost,
        clay_robot_cost,
        obsidian_robot_cost,
        geode_robot_cost,
    }
}

#[derive(Debug, Clone, Copy)]
struct State
{
    remaining: u32,
    score:     u32,
    resources: Point,
    rate:      Point,
}

impl State
{
    fn new(minutes_remaining: u32) -> Self
    {
        Self {
            remaining: minutes_remaining,
            score:     0,
            resources: Default::default(),
            rate:      (1, 0, 0),
        }
    }
}

fn pick_robot(state: State, cost: Point, robot: Point) -> Option<State>
{
    let plus = |r1: Point, r2: Point| (r1.0 + r2.0, r1.1 + r2.1, r1.2 + r2.2);
    let prod = |r1: Point, c: u32| (r1.0 * c, r1.1 * c, r1.2 * c);

    (1..state.remaining).rev().find_map(|minutes_remaining| {
        let minutes_passed = state.remaining - minutes_remaining - 1;
        let resources = plus(state.resources, prod(state.rate, minutes_passed));
        let resources_rate = plus(state.rate, robot);

        checked_sub(resources, cost).map(|resources| State {
            remaining: minutes_remaining,
            resources: plus(resources, state.rate),
            rate: resources_rate,
            ..state
        })
    })
}

fn branch(state: State, blueprint: &Blueprint) -> impl Iterator<Item = State> + '_
{
    let max = std::cmp::max(
        blueprint.clay_robot_cost.0,
        std::cmp::max(blueprint.obsidian_robot_cost.0, blueprint.geode_robot_cost.0),
    );

    [
        (state.rate.0 < max).then(|| pick_robot(state, blueprint.ore_robot_cost, (1, 0, 0))),
        (state.rate.1 < blueprint.obsidian_robot_cost.1)
            .then(|| pick_robot(state, blueprint.clay_robot_cost, (0, 1, 0))),
        (state.rate.2 < blueprint.geode_robot_cost.2 && state.rate.1 > 0)
            .then(|| pick_robot(state, blueprint.obsidian_robot_cost, (0, 0, 1))),
        (state.rate.2 > 0).then(|| {
            pick_robot(state, blueprint.geode_robot_cost, (0, 0, 0)).map(|state| State {
                score: state.score + state.remaining,
                ..state
            })
        }),
    ]
    .into_iter()
    .flatten()
    .flatten()
}

fn bound(state: State, blueprint: &Blueprint) -> u32
{
    let geode_cost = blueprint.geode_robot_cost.2;

    let mut obsidian = state.resources.2;
    let mut rate = state.rate.2;
    let mut geodes = state.score;

    for t in (0..state.remaining).rev()
    {
        if obsidian >= geode_cost
        {
            obsidian = obsidian + rate - geode_cost;
            geodes += t;
        }
        else
        {
            obsidian += rate;
            rate += 1;
        }
    }

    geodes
}

fn branch_and_bound(blueprint: &Blueprint, state: State, best: &mut u32)
{
    *best = std::cmp::max(*best, state.score);
    for state in branch(state, blueprint)
    {
        if bound(state, blueprint) > *best
        {
            branch_and_bound(blueprint, state, best);
        }
    }
}

fn solve<const N: u32>(blueprint: &Blueprint) -> u32
{
    let mut best = 0;
    branch_and_bound(&blueprint, State::new(N), &mut best);
    best
}

fn task_one(input: &[String]) -> u32
{
    input
        .iter()
        .enumerate()
        .map(|(id, line)| {
            let bp = blueprint3(line, (id + 1) as u32);
            bp.id * solve::<24>(&bp)
        })
        .sum()
}

fn task_two(input: &[String]) -> u32
{
    input
        .iter()
        .take(3)
        .enumerate()
        .map(|(id, line)| {
            let bp = blueprint3(line, (id + 1) as u32);
            solve::<32>(&bp)
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
