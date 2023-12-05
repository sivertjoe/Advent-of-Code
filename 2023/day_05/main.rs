use itertools::Itertools;

struct Map {
    dst: usize,
    src: usize,
    range: usize,
}
struct Seed(usize);
struct Function(Vec<Map>);

fn parse(input: &[String]) -> (Vec<Seed>, Vec<Function>) {
    let mut functions = Vec::new();
    let mut seeds = Vec::new();
    for inp in input.split(|line| line.is_empty()) {
        if inp[0].starts_with("seeds") {
            seeds = inp[0][6..]
                .split_ascii_whitespace()
                .map(|n| Seed(n.parse::<usize>().unwrap()))
                .collect::<Vec<_>>();
        } else {
            let next = inp
                .iter()
                .skip(1)
                .map(|line| {
                    let mut line = line
                        .split_ascii_whitespace()
                        .map(|w| w.parse::<usize>().unwrap());
                    let dst = line.next().unwrap();
                    let src = line.next().unwrap();
                    let range = line.next().unwrap();

                    Map { dst, src, range }
                })
                .collect::<Vec<_>>();
            functions.push(Function(next));
        }
    }
    (seeds, functions)
}

fn apply_function(seed: Seed, func: &Function) -> Seed {
    func.0
        .iter()
        .find_map(|map| {
            (map.src <= seed.0 && seed.0 < map.src + map.range)
                .then(|| Seed(seed.0 - map.src + map.dst))
        })
        .unwrap_or(seed)
}

fn apply_range(r: Vec<(usize, usize)>, func: &Function) -> Vec<(usize, usize)> {
    let mut res = Vec::new();
    let mut r = r;

    for Map { dst, src, range } in &func.0 {
        let end = *src + *range;

        let mut temp = Vec::new();

        for (st, ed) in r {
            let before = (st, ed.min(*src));
            let inter = (st.max(*src), end.min(ed));
            let after = (end.max(st), ed);
            if before.1 > before.0 {
                temp.push(before);
            }
            if inter.1 > inter.0 {
                res.push((inter.0 - *src + *dst, inter.1 - *src + *dst));
            }
            if after.1 > after.0 {
                temp.push(after);
            }
        }

        r = temp;
    }
    res.extend(r);
    res
}

fn task_one(input: &[String]) -> usize {
    let (seeds, fs) = parse(input);
    seeds
        .into_iter()
        .map(|seed| fs.iter().fold(seed, apply_function).0)
        .min()
        .unwrap()
}

fn task_two(input: &[String]) -> usize {
    let (seeds, fs) = parse(input);
    seeds
        .into_iter()
        .tuples()
        .flat_map(|(start, range)| {
            fs.iter()
                .fold(vec![(start.0, start.0 + range.0)], apply_range)
                .into_iter()
        })
        .map(|(st, _)| st)
        .min()
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
