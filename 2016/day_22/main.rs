use std::collections::*;

#[derive(Clone, Debug)]
struct Usage
{
    name:  String,
    used:  usize,
    avail: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct State
{
    goal_data: (usize, usize),
    blank:     (usize, usize),
}

fn ns(pos: (usize, usize)) -> Vec<(usize, usize)>
{
    let pos = (pos.0 as isize, pos.1 as isize);

    [(pos.0 + 1, pos.1), (pos.0 - 1, pos.1), (pos.0, pos.1 + 1), (pos.0, pos.1 - 1)]
        .into_iter()
        .filter(|(x, y)| *x >= 0 && *y >= 0)
        .map(|(x, y)| (x as usize, y as usize))
        .collect()
}

fn neighbors(state: &State, board: &HashSet<(usize, usize)>) -> Vec<State>
{
    ns(state.blank)
        .into_iter()
        .filter(|next| board.contains(next))
        .map(|next_blank| {
            let mut next_goal_data = state.goal_data;
            if next_blank == state.goal_data
            {
                next_goal_data = state.blank;
            };
            State {
                goal_data: next_goal_data, blank: next_blank
            }
        })
        .collect()
}

fn hcost(state: &State) -> usize
{
    manhattan_distance(state.goal_data, state.blank) + manhattan_distance(state.goal_data, (0, 0))
}

// Filesystem              Size  Used  Avail  Use%
// /dev/grid/node-x0-y0     92T   68T    24T   73%
fn parse_line(line: &String) -> (usize, usize, Usage)
{
    let mut iter = line.split_whitespace();
    let mut path = iter.next().unwrap().split('/');
    let name = path.nth(3).unwrap().to_string();

    let mut get = || {
        let s = iter.next().unwrap();
        s[0..s.len() - 1].parse::<usize>().unwrap()
    };
    let _ = get();
    let used = get();
    let avail = get();
    let _ = get();

    let mut iter = name.split('-');
    let x = iter.nth(1).unwrap();
    let x = x[1..].parse::<usize>().unwrap();

    let y = iter.next().unwrap();
    let y = y[1..].parse::<usize>().unwrap();

    (x, y, Usage {
        name,
        used,
        avail,
    })
}

fn parse(input: &[String]) -> Vec<(usize, usize, Usage)>
{
    input.iter().skip(2).map(parse_line).collect()
}

fn info(usages: &[(usize, usize, Usage)]) -> (State, HashSet<(usize, usize)>)
{
    let map = usages
        .iter()
        .cloned()
        .map(|(x, y, u)| ((x, y), u))
        .collect::<HashMap<(usize, usize), Usage>>();

    let mut blank: Option<(usize, usize)> = None;
    let mut tiles: HashSet<(usize, usize)> = HashSet::new();

    let len_x = map.keys().map(|(x, _)| *x).max().unwrap();

    for ((x, y), u) in map.iter()
    {
        let x = *x;
        let y = *y;
        if u.used == 0
        {
            blank = Some((x, y));
        }
        else
        {
            let is_tile = || {
                for (x0, y0) in
                    ns((x, y)).into_iter().filter(|(nx, ny)| map.contains_key(&(*nx, *ny)))
                {
                    let u2 = map.get(&(x0, y0)).unwrap();

                    if u.used > u2.used + u2.avail
                    {
                        return false;
                    }
                }
                true
            };
            if is_tile()
            {
                tiles.insert((x, y));
            }
        }
    }
    let blank = blank.unwrap();

    (
        State {
            goal_data: (len_x, 0),
            blank,
        },
        tiles,
    )
}

fn a_star(global: &HashSet<(usize, usize)>, start: &State) -> usize
{
    use std::cmp::Reverse;
    let mut heap = BinaryHeap::new();

    let init = (Reverse(hcost(start)), Reverse(0), start.clone());
    heap.push(init);

    let mut seen = BTreeSet::new();
    seen.insert(start.clone());

    while let Some((_h, cost, state)) = heap.pop()
    {
        if state.goal_data == (0, 0)
        {
            return cost.0;
        }
        for next_state in neighbors(&state, global)
        {
            let h = hcost(&next_state);
            let g = cost.0 + 1;
            let new = (Reverse(g + h), Reverse(g), next_state.clone());

            if seen.insert(next_state)
            {
                heap.push(new);
            }
        }
    }
    unreachable!()
}

fn manhattan_distance(p1: (usize, usize), p2: (usize, usize)) -> usize
{
    p1.0.abs_diff(p2.0) + p1.1.abs_diff(p2.1)
}

fn task_one(input: &[String]) -> usize
{
    let vec = input.iter().skip(2).map(parse_line).collect::<Vec<_>>();

    let v2 = vec.clone();
    let mut count = 0;
    for node in v2
    {
        for node2 in &vec
        {
            if node.2.name != node2.2.name && node.2.used != 0 && node.2.used <= node2.2.avail
            {
                count += 1;
            }
        }
    }
    count
}

fn task_two(input: &[String]) -> usize
{
    let usages = parse(input);

    let (start, tiles) = info(&usages);
    let board = tiles;
    a_star(&board, &start)
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
