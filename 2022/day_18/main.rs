use std::collections::*;

fn ns(p: (isize, isize, isize)) -> Vec<(isize, isize, isize)>
{
    [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]
        .into_iter()
        .map(|(x, y, z)| (p.0 + x, p.1 + y, p.2 + z))
        .collect()
}

fn parse(input: &[String]) -> HashSet<(isize, isize, isize)>
{
    let mut set = HashSet::new();
    for line in input
    {
        let mut nums = line.split(',').map(|tok| tok.parse::<isize>().unwrap());

        let mut n = || nums.next().unwrap();
        set.insert((n(), n(), n()));
    }
    set
}

fn task_one(input: &[String]) -> i32
{
    let set = parse(input);

    let mut res = 0;
    for p in set.iter()
    {
        for new in ns(*p)
        {
            if !set.contains(&new)
            {
                res += 1;
            }
        }
    }
    res
}


fn task_two(input: &[String]) -> isize
{
    let set = parse(input);

    let mut vec = VecDeque::new();
    vec.push_back((-1, -1, -1));

    let mut seen = HashSet::new();
    let mut sum = 0;

    // max(max_x, max_y, max_z) = 21, with 23 the water have room to flow axis 22
    const GRID_BOUNDS: isize = 23;

    // Traditionally 0 would be the smallest possible point; however, since some
    // points lie on this axis, we need to do one below to let water flow
    // freely.
    let in_grid = |p: (isize, isize, isize)| {
        p.0 < GRID_BOUNDS
            && p.0 >= -1
            && p.1 < GRID_BOUNDS
            && p.1 >= -1
            && p.2 < GRID_BOUNDS
            && p.2 >= -1
    };

    while let Some(point) = vec.pop_front()
    {
        if !seen.insert(point) || !in_grid(point)
        {
            continue;
        }

        for n in ns(point)
        {
            if set.contains(&n)
            {
                sum += 1;
            }
            else
            {
                vec.push_back(n);
            }
        }
    }
    sum
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
