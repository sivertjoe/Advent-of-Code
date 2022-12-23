use std::collections::*;
const MOV: [[(isize, isize); 3]; 4] = [
    [(0, -1), (1, -1), (-1, -1)],
    [(0, 1), (1, 1), (-1, 1)],
    [(-1, 0), (-1, -1), (-1, 1)],
    [(1, 0), (1, -1), (1, 1)],
];

fn propose_move(
    pos: (isize, isize),
    map: &HashSet<(isize, isize)>,
    mov: usize,
) -> Option<(isize, isize)>
{
    let mut seen = false;
    'outer: for y in -1..=1
    {
        for x in -1..=1
        {
            if y == 0 && x == 0
            {
                continue;
            }
            let np = (pos.0 + x, pos.1 + y);
            if map.contains(&np)
            {
                seen = true;
                break 'outer;
            }
        }
    }

    if !seen
    {
        return None;
    }

    for i in 0..4
    {
        let idx = (mov + i).rem_euclid(4);
        if MOV[idx].iter().all(|dp| !map.contains(&(pos.0 + dp.0, pos.1 + dp.1)))
        {
            let mm = MOV[idx][0];
            let np = (pos.0 + mm.0, pos.1 + mm.1);
            return Some(np);
        }
    }
    None
}

fn parse(input: &[String]) -> HashSet<(isize, isize)>
{
    let mut map = HashSet::new();
    for (y, line) in input.iter().enumerate()
    {
        for (x, ch) in line.chars().enumerate()
        {
            if ch == '#'
            {
                map.insert((x as isize, y as isize));
            }
        }
    }
    map
}

fn solve(input: &[String], early_exit: bool) -> usize
{
    let mut map = parse(input);

    for round in 0..
    {
        if early_exit && round == 10
        {
            break;
        }
        let mut new_map: HashSet<(isize, isize)> = HashSet::with_capacity(map.len());

        let mut vec = Vec::new();

        let mut len = 0;
        for k in map.iter()
        {
            if let Some(mov) = propose_move(*k, &map, round)
            {
                vec.push((k, mov));
            }
            else
            {
                new_map.insert(*k);
                len += 1;
            }
        }
        if len == map.len()
        {
            return round + 1;
        }

        let mut temp: HashMap<&(isize, isize), Vec<&&(isize, isize)>> = HashMap::new();
        for (from, to) in vec.iter()
        {
            temp.entry(to).or_default().push(from);
        }

        for (pos, vec) in temp.into_iter()
        {
            if vec.len() == 1
            {
                new_map.insert(*pos);
            }
            else
            {
                for from in vec
                {
                    new_map.insert(**from);
                }
            }
        }

        map = new_map;
    }

    let mut res = 0;
    let (x, y, w, h) = bounding_box(&map);
    for y in y..=y + h
    {
        for x in x..=x + w
        {
            if !map.contains(&(x, y))
            {
                res += 1;
            }
        }
    }

    res
}

fn bounding_box(map: &HashSet<(isize, isize)>) -> (isize, isize, isize, isize)
{
    let min_y = map.iter().map(|(_x, y)| *y).min().unwrap();
    let min_x = map.iter().map(|(x, _y)| *x).min().unwrap();

    let max_y = map.iter().map(|(_x, y)| *y).max().unwrap();
    let max_x = map.iter().map(|(x, _y)| *x).max().unwrap();

    (min_x, min_y, max_x - min_x, max_y - min_y)
}

fn task_one(input: &[String]) -> usize
{
    solve(input, true)
}

fn task_two(input: &[String]) -> usize
{
    solve(input, false)
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
