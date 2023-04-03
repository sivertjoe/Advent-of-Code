use std::{collections::*, iter::FromIterator};

#[derive(Debug, Clone)]
struct Room
{
    rate:  i32,
    exits: Vec<String>,
}

fn parse_data(lines: &[String]) -> HashMap<String, Room>
{
    let mut rooms = HashMap::new();

    for line in lines
    {
        let line = line.replace(',', "");
        let line = line.split_whitespace().collect::<Vec<_>>();
        if line.is_empty()
        {
            continue;
        }
        let source = line[1];
        let rate = line[4][5..line[4].len() - 1].parse().unwrap();
        let exits = line[9..].to_vec();
        rooms.insert(source.to_string(), Room {
            rate,
            exits: exits.iter().map(|s| s.to_string()).collect(),
        });
    }
    rooms
}

fn find_distances(rooms: &HashMap<String, Room>) -> (HashMap<(&str, &str), i32>, BTreeSet<&str>)
{
    let key_rooms = rooms
        .iter()
        .filter_map(|(k, v)| (v.rate > 0 || k == "AA").then_some(k.as_str()))
        .collect::<BTreeSet<&str>>();

    let mut distances: HashMap<(&str, &str), i32> = HashMap::new();

    for start_room in rooms.keys()
    {
        if !key_rooms.contains(start_room.as_str())
        {
            continue;
        }

        let mut cur: BTreeSet<&str> = BTreeSet::new();
        cur.insert(start_room);
        let mut next: BTreeSet<&str> = BTreeSet::new();
        let mut dist = 0;

        distances.insert((start_room, start_room), 0);

        while !cur.is_empty()
        {
            dist += 1;
            for pos in cur
            {
                for newpos in &rooms[pos].exits
                {
                    if let std::collections::hash_map::Entry::Vacant(e) =
                        distances.entry((start_room, newpos))
                    {
                        e.insert(dist);
                        next.insert(newpos);
                    }
                }
            }
            cur = next;
            next = BTreeSet::new();
        }
    }
    (distances, key_rooms)
}

fn find_best_total_flow(
    cur: &str,
    time: i32,
    seen: &BTreeSet<&str>,
    targets: &BTreeSet<&str>,
    rooms: &HashMap<String, Room>,
    distances: &HashMap<(&str, &str), i32>,
) -> i32
{
    let mut seen = seen.clone();
    seen.insert(cur);

    let targets = targets.difference(&seen).copied().collect::<BTreeSet<&str>>();

    let mut best_flow = 0;
    for target in targets.iter()
    {
        let time_left = time - distances[&(cur, *target)] - 1;
        if time_left > 0
        {
            let flow = rooms[*target].rate * time_left;
            let flow =
                flow + find_best_total_flow(target, time_left, &seen, &targets, rooms, distances);
            if flow > best_flow
            {
                best_flow = flow;
            }
        }
    }
    best_flow
}


fn task_one(input: &[String]) -> i32
{
    let rooms = parse_data(input);
    let (distances, key_rooms) = find_distances(&rooms);

    find_best_total_flow("AA", 30, &BTreeSet::new(), &key_rooms, &rooms, &distances)
}

#[allow(clippy::too_many_arguments)]
fn find_and_record<'a>(
    cur: &'a str,
    curflow: i32,
    time: i32,
    seen: BTreeSet<&'a str>,
    rooms: &HashMap<&str, Room>,
    key_rooms: &BTreeSet<&'a str>,
    distances: &HashMap<(&str, &str), i32>,
    endpoints: &mut HashMap<BTreeSet<&'a str>, i32>,
) -> i32
{
    let mut seen = seen;
    seen.insert(cur);
    let targets = BTreeSet::from_iter(key_rooms.difference(&seen));

    let start = BTreeSet::from_iter(["AA"]);
    let torecord = BTreeSet::from_iter(seen.difference(&start).copied());

    if endpoints.contains_key(&torecord)
    {
        endpoints.insert(torecord.clone(), i32::max(endpoints[&torecord], curflow));
    }
    else
    {
        endpoints.insert(torecord, curflow);
    }

    let mut best_flow = 0;
    for target in targets
    {
        let time_left = time - distances[&(cur, *target)] - 1;
        if time_left > 0
        {
            let newflow = rooms[target].rate * time_left;
            let newflow = newflow
                + find_and_record(
                    target,
                    curflow + newflow,
                    time_left,
                    seen.clone(),
                    rooms,
                    key_rooms,
                    distances,
                    endpoints,
                );
            if newflow > best_flow
            {
                best_flow = newflow;
            }
        }
    }
    best_flow
}

fn fill_in_endpoints<'a>(
    cur: BTreeSet<&'a str>,
    endpoints: &mut HashMap<BTreeSet<&'a str>, i32>,
) -> i32
{
    if !endpoints.contains_key(&cur)
    {
        let mut best_flow = 0;
        for e in cur.iter()
        {
            let mut subset = cur.clone();
            subset.remove(e);
            let new_flow = fill_in_endpoints(subset, endpoints);
            if new_flow > best_flow
            {
                best_flow = new_flow;
            }
        }
        endpoints.insert(cur.clone(), best_flow);
    }
    *endpoints.get(&cur).unwrap()
}


fn task_two(input: &[String]) -> i32
{
    let rooms = parse_data(input);
    let (distances, key_rooms) = find_distances(&rooms);

    let mut endpoints = HashMap::new();

    let rooms: HashMap<&str, Room> = rooms.iter().map(|(k, v)| (k.as_str(), v.clone())).collect();
    find_and_record("AA", 0, 26, BTreeSet::new(), &rooms, &key_rooms, &distances, &mut endpoints);

    let mut curr = key_rooms.clone();
    curr.remove("AA");
    fill_in_endpoints(curr, &mut endpoints);


    let mut best_flow = 0;
    for human_work in endpoints.keys()
    {
        let mut elephant_work = key_rooms.clone();
        elephant_work.remove("AA");
        let elephant_work = elephant_work.difference(human_work).copied().collect();

        let total_flow = endpoints[human_work] + endpoints[&elephant_work];
        if total_flow > best_flow
        {
            best_flow = total_flow;
        }
    }
    best_flow
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
