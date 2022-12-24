use std::collections::*;

#[derive(Clone, Copy, Eq,PartialEq, Ord, PartialOrd)]
enum Dir 
{
    Up,
    Down,
    Left, 
    Right
}

#[derive(Clone, Eq,PartialEq, Ord, PartialOrd)]
enum Item 
{
    Wall,
    Open(Vec<Dir>)
}

fn bounding_box(map: &BTreeMap<(usize, usize), Item>) -> (usize, usize, usize, usize)
{
    let min_y = map.keys().map(|(_x, y)| *y).min().unwrap();
    let min_x = map.keys().map(|(x, _y)| *x).min().unwrap();

    let max_y = map.keys().map(|(_x, y)| *y).max().unwrap();
    let max_x = map.keys().map(|(x, _y)| *x).max().unwrap();

    (min_x, min_y, max_x - min_x, max_y - min_y)
}

fn print(map: &BTreeMap<(usize, usize), Item>)
{
    let (x, y, w, h) = bounding_box(map);
    for y in y..=y+h
    {
        for x in x..=x+w
        {
            match map.get(&(x, y)).unwrap()
            {
                Item::Wall => print!("#"),
                Item::Open(s) => 
                {
                    if s.is_empty() {
                        print!(".");
                    }
                    else if s.len() == 1
                    {
                        let ch = match &s[0]
                        {
                            Dir::Left => '<',
                            Dir::Right => '>',
                            Dir::Up => '^',
                            Dir::Down => 'v'
                        };
                        print!("{}", ch);
                    }
                    else {
                        print!("{}", s.len());
                    }
                }
            }
        }
        println!();
    }
    println!();
}

fn step(map: &mut BTreeMap<(usize, usize), Item>)
{
    let keys = map.keys().map(|s| *s).collect::<Vec<_>>();

    let max_y = map.keys().map(|(_x, y)| *y).max().unwrap();
    let max_x = map.keys().map(|(x, _y)| *x).max().unwrap();

    let mut res = Vec::new();
    for (x, y) in keys
    {
        if let Some(Item::Open(space)) = map.get(&(x, y)) 
        {
            for blizz in space.iter()
            {
                let next = match blizz
                {
                    Dir::Up => (x, y - 1),
                    Dir::Down => (x, y + 1),
                    Dir::Left => (x-1, y),
                    Dir::Right => (x+1, y),
                };
                
                if *map.get(&next).unwrap() == Item::Wall
                {
                    let next = match blizz
                    {
                        Dir::Up => (x, max_y-1),
                        Dir::Down => (x, 1),
                        Dir::Left => (max_x-1, y),
                        Dir::Right => (1, y),
                    };
                    res.push((next, *blizz));
                }
                if matches!(*map.get(&next).unwrap(),Item::Open(_))
                {
                    res.push((next, *blizz));
                }

            }
        }
        if let Some(Item::Open(ref mut space)) = map.get_mut(&(x, y)) 
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

fn neighbors(state: (usize, usize), board: &BTreeMap<(usize, usize), Item>) -> Vec<(usize, usize)>
{
    ns(state)
        .into_iter()
        .filter(|next| matches!(board.get(&next), Some(Item::Open(v)) if v.is_empty()))
        .collect()
}

fn hcost(state: (usize, usize), end: (usize, usize)) -> usize
{
    manhattan_distance(state, end)
}


fn a_star(start: (usize, usize), end: (usize, usize), map: &BTreeMap<(usize,usize), Item>) -> (usize, BTreeMap<(usize, usize), Item>)
{
    use std::cmp::Reverse;
    let mut heap = BinaryHeap::new();

    let init = (Reverse(hcost(start, end)), Reverse(0), start);
    heap.push(init);

    let mut seen = BTreeSet::new();
    seen.insert((start, 0));


    let mut cache: HashMap<usize, BTreeMap<(usize, usize), Item>> = HashMap::new();
    let mut map = map.clone();
    step(&mut map);
    cache.insert(0, map);



    while let Some((_h, cost, state)) = heap.pop()
    {
        if state == end
        {
            let map = cache.get(&cost.0).unwrap().clone();
            return (cost.0 +1, map);
        }


        let nc = cost.0 + 1;
        if !cache.contains_key(&nc)
        {
            let mut prev = cache.get(&cost.0).unwrap().clone();
            step(&mut prev);
            cache.insert(nc, prev);
        }

        let map = cache.get(&nc).unwrap();
        for next_state in neighbors(state, &map)
        {
            if seen.insert((next_state, nc))
            {
                let h = hcost(next_state, end);
                let g = cost.0 + 1;
                let new = (Reverse(g + h), Reverse(g), next_state.clone());
                heap.push(new);
            }
        }
        if matches!(map.get(&state), Some(Item::Open(v)) if v.is_empty()) && seen.insert((state, nc))
        {
            let h = hcost(state, end);
            let g = cost.0 + 1;
            let new = (Reverse(g + h), Reverse(g), state.clone());
            heap.push(new);
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
    let mut map = BTreeMap::new();
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

    let start: (usize, usize) = {
        *map.iter().find(|((x, y), ch)| *y == 0 && matches!(**ch, Item::Open(_))).unwrap().0
    };
    let end: (usize, usize) = {
        *map.iter().find(|((x, y), ch)| *y == max_y && matches!(**ch, Item::Open(_))).unwrap().0
    };
    a_star(start, end, &map).0
}

fn task_two(input: &[String]) -> usize
{
    let mut map = BTreeMap::new();
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

    let start: (usize, usize) = {
        *map.iter().find(|((x, y), ch)| *y == 0 && matches!(**ch, Item::Open(_))).unwrap().0
    };
    let end: (usize, usize) = {
        *map.iter().find(|((x, y), ch)| *y == max_y && matches!(**ch, Item::Open(_))).unwrap().0
    };

    let mut res = 0;
    let (cost, map) = a_star(start, end, &map);
    res += cost;

    let (cost, map) = a_star(end, start, &map);
    res += cost;

    let (cost, map) = a_star(start, end, &map);
    res += cost;

    res
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
