use std::collections::*;

fn neighbors<'a, Cmp>(
    (x, y): (usize, usize),
    curr: u8,
    grid: &'a [Vec<u8>],
    cmp: Cmp,
) -> impl Iterator<Item = (usize, usize)> + '_
where
    Cmp: Fn(u8, u8) -> bool + 'a,
{
    let in_grid =
        |x, y| x >= 0 && (x as usize) < grid[0].len() && y >= 0 && (y as usize) < grid.len();

    let x = x as isize;
    let y = y as isize;
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        .into_iter()
        .filter_map(move |(x, y)| {
            (in_grid(x, y) && cmp(curr, grid[y as usize][x as usize]))
                .then_some((x as usize, y as usize))
        })
}

fn bfs<Goal, Cmp>(starting: (usize, usize), grid: &[Vec<u8>], goal: Goal, cmp: Cmp) -> usize
where
    Goal: Fn((usize, usize)) -> bool,
    Cmp: Fn(u8, u8) -> bool,
{
    let mut vec = VecDeque::new();
    let mut seen = HashSet::new();

    vec.push_back((0, starting));
    seen.insert(starting);

    while let Some((cost, pos)) = vec.pop_front()
    {
        let curr = grid[pos.1][pos.0];
        if goal(pos)
        {
            return cost;
        }

        for neighbor in neighbors(pos, curr, grid, &cmp)
        {
            if seen.insert(neighbor)
            {
                vec.push_back((cost + 1, neighbor));
            }
        }
    }
    unreachable!()
}

fn get_grid(input: &[String]) -> Vec<Vec<u8>>
{
    input.iter().map(|line| line.as_bytes().to_vec()).collect()
}

fn find(vec: &[Vec<u8>], target: u8) -> (usize, usize)
{
    for y in 0..vec.len()
    {
        for x in 0..vec[y].len()
        {
            if vec[y][x] == target
            {
                return (x, y);
            }
        }
    }
    unreachable!()
}

fn task_one(input: &[String]) -> usize
{
    let mut vec = get_grid(input);

    let target = find(&vec, b'E');
    vec[target.1][target.0] = b'z';

    let start = find(&vec, b'S');
    vec[start.1][start.0] = b'a';

    bfs(start, &vec, |pos| pos == target, |curr, next| curr + 1 >= next)
}

fn task_two(input: &[String]) -> usize
{
    let vec = get_grid(input);
    let end = find(&vec, b'E');
    bfs(end, &vec, |(x, y)| vec[y][x] == b'a', |curr, next| curr - 1 <= next)
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
