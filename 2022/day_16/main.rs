use std::collections::*;

fn task_one(input: &[String]) -> usize
{
    let mut pressure = HashMap::new();
    let mut map: HashMap<String, Vec<String>> = HashMap::new();

    for line in input
    {
        let (press, path) = line.split_once(';').unwrap();
        let mut iter = press.split_whitespace();
        let name = iter.nth(1).unwrap();
        let press: &str = iter.nth(2).unwrap();
        let press = press.trim_start_matches("rate=");
        let press = press.parse::<usize>().unwrap();

        pressure.insert(name.to_string(), press);

        let iter = path.split_whitespace();
        for elem in iter.skip(4).map(|w| w.trim_end_matches(','))
        {
            map.entry(name.to_string()).or_default().push(elem.to_string());
        }
    }

    rec("AA", 0, 0, 1, &map, &pressure, HashSet::new(), &mut HashMap::new()).unwrap()
}

fn rec(
    pos: &str,
    flow_rate: usize,
    res: usize,
    minutes: usize,
    map: &HashMap<String, Vec<String>>,
    pressure: &HashMap<String, usize>,
    visited: HashSet<String>,
    cache: &mut HashMap<(usize, String, usize), usize>,
) -> Option<usize>
{
    if minutes > 30
    {
        return Some(res);
    }
    let cache_key = (minutes, pos.to_string(), flow_rate);
    if let Some(cache_res) = cache.get(&cache_key)
    {
        if *cache_res >= res
        {
            return None;
        }
    }
    cache.insert(cache_key, res);

    let rate = pressure[pos];

    let next_best_open = if rate > 0 && !visited.contains(pos)
    {
        let mut visited = visited.clone();
        visited.insert(pos.to_string());
        rec(pos, flow_rate + rate, res + flow_rate, minutes + 1, map, pressure, visited, cache)
    }
    else
    {
        None
    };

    let next_best_down = map[pos]
        .iter()
        .filter_map(|new_pos| {
            rec(
                new_pos,
                flow_rate,
                res + flow_rate,
                minutes + 1,
                map,
                pressure,
                visited.clone(),
                cache,
            )
        })
        .max();

    std::cmp::max(next_best_open, next_best_down)
}

fn task_two(input: &[String]) -> usize
{
    let mut pressure = HashMap::new();
    let mut map: HashMap<String, Vec<String>> = HashMap::new();

    for line in input
    {
        let (press, path) = line.split_once(';').unwrap();
        let mut iter = press.split_whitespace();
        let name = iter.nth(1).unwrap();
        let press: &str = iter.nth(2).unwrap();
        let press = press.trim_start_matches("rate=");
        let press = press.parse::<usize>().unwrap();

        pressure.insert(name.to_string(), press);

        let iter = path.split_whitespace();
        for elem in iter.skip(4).map(|w| w.trim_end_matches(','))
        {
            map.entry(name.to_string()).or_default().push(elem.to_string());
        }
    }

    rec2(1, "AA", "AA", 0, 0, &map, &pressure, &HashSet::new(), &mut HashMap::new()).unwrap()
}

fn rec2(
    minutes: usize,
    my_pos: &str,
    elephant_pos: &str,
    flow_rate: usize,
    score: usize,
    map: &HashMap<String, Vec<String>>,
    pressure: &HashMap<String, usize>,
    visited: &HashSet<String>,
    cache: &mut HashMap<(usize, String, String, usize), usize>,
) -> Option<usize>
{
    if minutes > 26
    {
        return Some(score);
    }

    let cache_key = (minutes, my_pos.to_string(), elephant_pos.to_string(), flow_rate);
    if let Some(cache_res) = cache.get(&cache_key)
    {
        if *cache_res >= score
        {
            return None;
        }
    }

    cache.insert(cache_key, score);

    let (my_flow_rate, my_tunnels) = { (pressure[my_pos], &map[my_pos]) };
    let (elephant_flow_rate, elephant_tunnels) = { (pressure[elephant_pos], &map[elephant_pos]) };

    let can_open_my_valve = my_flow_rate > 0 && !visited.contains(my_pos);
    let can_open_elephant_valve = elephant_flow_rate > 0 && !visited.contains(elephant_pos);

    let mut res = Vec::new();
    if can_open_my_valve
    {
        let mut visited = visited.clone();
        visited.insert(my_pos.to_string());
        for ep in elephant_tunnels.iter()
        {
            res.push(rec2(
                minutes + 1,
                my_pos,
                ep,
                flow_rate + my_flow_rate,
                score + flow_rate,
                map,
                pressure,
                &visited,
                cache,
            ));
        }
    }

    if can_open_elephant_valve
    {
        let mut visited = visited.clone();
        visited.insert(elephant_pos.to_string());
        for mp in my_tunnels.iter()
        {
            res.push(rec2(
                minutes + 1,
                mp,
                elephant_pos,
                flow_rate + elephant_flow_rate,
                score + flow_rate,
                map,
                pressure,
                &visited,
                cache,
            ));
        }
    }

    if can_open_elephant_valve && can_open_my_valve && my_pos != elephant_pos
    {
        let mut visited = visited.clone();
        visited.insert(elephant_pos.to_string());
        visited.insert(my_pos.to_string());

        res.push(rec2(
            minutes + 1,
            my_pos,
            elephant_pos,
            flow_rate + elephant_flow_rate + my_flow_rate,
            score + flow_rate,
            map,
            pressure,
            &visited,
            cache,
        ));
    }

    for new_elephant_location in elephant_tunnels.iter()
    {
        for new_my_location in my_tunnels.iter()
        {
            res.push(rec2(
                minutes + 1,
                new_my_location,
                new_elephant_location,
                flow_rate,
                score + flow_rate,
                map,
                pressure,
                visited,
                cache,
            ));
        }
    }

    res.into_iter().flatten().max()
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
