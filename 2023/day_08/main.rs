type HashMap<K, V> = std::collections::HashMap<K, V, fxhash::FxBuildHasher>;

fn parse(input: &[String]) -> (String, HashMap<String, [String; 2]>) {
    let path = input[0].to_owned();
    let mut map = HashMap::default();
    for line in input[2..].iter() {
        let line = line.replace(['(', ')'], "");
        let (key, rest) = line.split_once(" = ").unwrap();
        let (a, b) = rest.split_once(", ").unwrap();

        map.insert(key.to_string(), [a.to_string(), b.to_string()]);
    }
    (path, map)
}

fn calc_steps(p: &str, start: &String, map: &HashMap<String, [String; 2]>) -> usize {
    let mut count = 0;

    let mut curr = start;
    for idx in p.bytes().map(|b| if b == b'L' { 0 } else { 1 }).cycle() {
        count += 1;
        let next = &map.get(curr).unwrap()[idx];
        if next.ends_with('Z') {
            return count;
        }

        curr = next;
    }

    unreachable!()
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(a: usize, b: usize) -> usize {
    (a * b) / gcd(a, b)
}

fn task_one(input: &[String]) -> usize {
    let (p, map) = parse(input);
    calc_steps(&p, &"AAA".to_string(), &map)
}

fn task_two(input: &[String]) -> usize {
    let (p, map) = parse(input);

    map.keys()
        .filter(|k| k.ends_with('A'))
        .map(|k| calc_steps(&p, k, &map))
        .reduce(lcm)
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
