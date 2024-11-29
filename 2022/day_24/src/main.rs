use std::collections::*;

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
enum Dir
{
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
enum Item
{
    Wall,
    Open(Vec<Dir>),
}

fn step(map: &mut HashMap<(usize, usize), Item>, max_x: usize, max_y: usize)
{
    let mut res = Vec::new();

    for (x, y) in map.keys().copied()
    {
        if let Some(Item::Open(space)) = map.get(&(x, y))
        {
            for blizz in space.iter()
            {
                let next = match blizz
                {
                    Dir::Up => (x, y - 1),
                    Dir::Down => (x, y + 1),
                    Dir::Left => (x - 1, y),
                    Dir::Right => (x + 1, y),
                };

                match map.get(&next).unwrap()
                {
                    Item::Wall =>
                    {
                        let next = match blizz
                        {
                            Dir::Up => (x, max_y - 1),
                            Dir::Down => (x, 1),
                            Dir::Left => (max_x - 1, y),
                            Dir::Right => (1, y),
                        };
                        res.push((next, *blizz));
                    },
                    Item::Open(_) =>
                    {
                        res.push((next, *blizz));
                    },
                };
            }
        }
    }
    for (_, item) in map.iter_mut()
    {
        if let Item::Open(ref mut space) = item
        {
            space.clear();
        }
    }

    for (key, blizz) in res
    {
        if let Some(Item::Open(ref mut space)) = map.get_mut(&key)
        {
            space.push(blizz);
        }
    }
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

fn neighbors(state: (usize, usize), board: &HashMap<(usize, usize), Item>) -> Vec<(usize, usize)>
{
    ns(state)
        .into_iter()
        .filter(|next| matches!(board.get(next), Some(Item::Open(v)) if v.is_empty()))
        .collect()
}

fn hcost(state: (usize, usize), end: (usize, usize)) -> usize
{
    manhattan_distance(state, end)
}

fn a_star(
    start: (usize, usize),
    end: (usize, usize),
    map: &HashMap<(usize, usize), Item>,
) -> (usize, HashMap<(usize, usize), Item>)
{
    let max_y = map.keys().map(|(_x, y)| *y).max().unwrap();
    let max_x = map.keys().map(|(x, _y)| *x).max().unwrap();

    use std::cmp::Reverse;
    let mut heap = BinaryHeap::new();

    let init = (Reverse(hcost(start, end)), Reverse(0), start);
    heap.push(init);

    let mut seen = HashSet::new();
    seen.insert((start, 0));

    let mut cache: HashMap<usize, HashMap<(usize, usize), Item>> = HashMap::new();
    let mut map = map.clone();
    step(&mut map, max_x, max_y);
    cache.insert(0, map);

    while let Some((_h, cost, state)) = heap.pop()
    {
        if state == end
        {
            let map = cache.get(&cost.0).unwrap().clone();
            return (cost.0 + 1, map);
        }

        let nc = cost.0 + 1;
        if !cache.contains_key(&nc)
        {
            let mut prev = cache.get(&cost.0).unwrap().clone();
            step(&mut prev, max_x, max_y);
            cache.insert(nc, prev);
        }

        let map = cache.get(&nc).unwrap();
        for next_state in neighbors(state, map)
        {
            if seen.insert((next_state, nc))
            {
                let h = hcost(next_state, end);
                let g = cost.0 + 1;
                let new = (Reverse(g + h), Reverse(g), next_state);
                heap.push(new);
            }
        }
        if matches!(map.get(&state), Some(Item::Open(v)) if v.is_empty())
            && seen.insert((state, nc))
        {
            let h = hcost(state, end);
            let g = cost.0 + 1;
            let new = (Reverse(g + h), Reverse(g), state);
            heap.push(new);
        }
    }
    unreachable!()
}

fn manhattan_distance(p1: (usize, usize), p2: (usize, usize)) -> usize
{
    p1.0.abs_diff(p2.0) + p1.1.abs_diff(p2.1)
}

fn parse(input: &[String]) -> (HashMap<(usize, usize), Item>, (usize, usize), (usize, usize))
{
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate()
    {
        for (x, ch) in line.chars().enumerate()
        {
            let item = match ch
            {
                '#' => Item::Wall,
                '.' => Item::Open(Vec::new()),
                '>' => Item::Open(vec![Dir::Right]),
                '<' => Item::Open(vec![Dir::Left]),
                '^' => Item::Open(vec![Dir::Up]),
                'v' => Item::Open(vec![Dir::Down]),
                _ => unreachable!(),
            };
            map.insert((x, y), item);
        }
    }
    let max_y = map.keys().map(|(_x, y)| *y).max().unwrap();
    let max_x = map.keys().map(|(x, _y)| *x).max().unwrap();
    (map, (1, 0), (max_x - 1, max_y))
}

fn task_one(input: &[String]) -> usize
{
    let (map, start, end) = parse(input);
    a_star(start, end, &map).0
}

fn task_two(input: &[String]) -> usize
{
    let (map, start, end) = parse(input);


    (0..3)
        .fold((0, start, end, map), |(res, start, end, map), _| {
            let (cost, map) = a_star(start, end, &map);
            (res + cost, end, start, map)
        })
        .0
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
