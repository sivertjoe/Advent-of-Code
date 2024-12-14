type P = (i32, i32);
type HashMap<K, V> = fxhash::FxHashMap<K, V>;
type Vec<T> = smallvec::SmallVec<[T; 8]>;

const W: i32 = 101;
const H: i32 = 103;

fn parse(input: &[String]) -> Vec<(P, P)> {
    let get = |iter: &mut dyn Iterator<Item = i32>| iter.next().unwrap();

    let mut vec = Vec::with_capacity(input.len());
    for line in input {
        let line = line.replace("p=", "").replace("v=", "");

        let mut split = line
            .split_whitespace()
            .map(|v| v.split(',').map(|n| n.parse::<i32>().unwrap()));
        let mut fst = split.next().unwrap();
        let mut snd = split.next().unwrap();

        vec.push((
            (get(&mut fst), get(&mut fst)),
            (get(&mut snd), get(&mut snd)),
        ));
    }
    vec
}

fn update(map: &HashMap<P, Vec<P>>, times: i32) -> HashMap<P, Vec<P>> {
    let mut new: HashMap<P, Vec<P>> =
        HashMap::with_capacity_and_hasher(map.len(), fxhash::FxBuildHasher::default());

    for (pos, robots) in map {
        for robot_vel in robots {
            let new_pos = (
                (pos.0 + times * robot_vel.0).rem_euclid(H as i32),
                (pos.1 + times * robot_vel.1).rem_euclid(W as i32),
            );
            new.entry(new_pos).or_default().push(*robot_vel);
        }
    }

    new
}

fn get_map(input: &[String]) -> HashMap<P, Vec<P>> {
    let vec = parse(input);

    let mut map: HashMap<P, Vec<P>> =
        HashMap::with_capacity_and_hasher(vec.len(), fxhash::FxBuildHasher::default());

    for robot in vec {
        map.entry((robot.0 .1, robot.0 .0))
            .or_default()
            .push((robot.1 .1, robot.1 .0));
    }
    map
}

fn contains_xmastree(map: &HashMap<P, Vec<P>>) -> bool {
    map.values().all(|v| v.len() == 1)
}

fn task_one(input: &[String]) -> usize {
    let mut map = get_map(input);
    map = update(&map, 100);

    let qs = [
        (0..H / 2, 0..W / 2),
        ((0..H / 2), (W / 2) + 1..W),
        ((H / 2) + 1..H, 0..W / 2),
        ((H / 2) + 1..H, (W / 2) + 1..W),
    ];

    let mut quads = [0, 0, 0, 0];

    for (pos, vec) in map {
        if let Some(idx) = qs
            .iter()
            .position(|(y, x)| y.contains(&pos.0) && x.contains(&pos.1))
        {
            quads[idx] += vec.len();
        }
    }

    quads.into_iter().product()
}

fn task_two(input: &[String]) -> usize {
    use rayon::prelude::*;
    let map = get_map(input);

    (1..10_000)
        .into_par_iter()
        .find_first(|i| contains_xmastree(&update(&map, *i as _)))
        .unwrap()
}

fn main() {
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn read_input<P>(path: P) -> Vec<String>
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

enum Task {
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
    let elapsed = t.elapsed();
    let fmt = std::env::var("TASKUNIT").unwrap_or("ms".to_owned());

    let (u, elapsed) = match fmt.as_str() {
        "ms" => ("ms", elapsed.as_millis()),
        "ns" => ("ns", elapsed.as_nanos()),
        "us" => ("μs", elapsed.as_micros()),
        "s" => ("s", elapsed.as_secs() as u128),
        _ => panic!("unsupported time format"),
    };

    match task {
        Task::One => {
            println!("({}{u})\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        }
        Task::Two => {
            println!("({}{u})\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        }
    };
}

fn get_input_file() -> String {
    std::env::args()
        .nth(1)
        .unwrap_or_else(|| "input".to_string())
}
