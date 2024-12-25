use std::collections::*;

#[derive(Debug, Clone, PartialEq)]
enum Operation {
    AND,
    OR,
    XOR,
}

#[derive(Debug, Clone)]
struct Gate {
    in1: String,
    in2: String,
    out: String,
    oper: Operation,
}

impl From<String> for Gate {
    fn from(value: String) -> Self {
        let mut iter = value.split_ascii_whitespace();
        let in1 = iter.next().unwrap().to_owned();

        let oper = match iter.next().unwrap() {
            "OR" => Operation::OR,
            "XOR" => Operation::XOR,
            "AND" => Operation::AND,
            _ => unreachable!(),
        };
        let in2 = iter.next().unwrap().to_owned();
        let out = iter.nth(1).unwrap().to_owned();

        Gate {
            in1,
            in2,
            oper,
            out,
        }
    }
}

fn parse(input: &[String]) -> (HashMap<String, bool>, Vec<Gate>) {
    let mut iter = input.split(|line| line.is_empty());

    let fst = iter.next().unwrap();
    let mut map = HashMap::new();
    for line in fst {
        let (a, b) = line.split_once(": ").unwrap();
        map.insert(
            a.to_string(),
            if b.parse::<u8>().unwrap() == 0 {
                false
            } else {
                true
            },
        );
    }

    let snd = iter.next().unwrap();
    let oper = snd
        .into_iter()
        .map(|line| Gate::from(line.clone()))
        .collect::<Vec<_>>();
    (map, oper)
}

fn convert(map: &HashMap<String, bool>, ch: char) -> isize {
    let mut vec = map
        .into_iter()
        .filter(|(name, _val)| name.starts_with(ch))
        .collect::<Vec<_>>();

    vec.sort_by_key(|k| k.0.clone());
    let s = vec
        .into_iter()
        .rev()
        .map(|(_name, val)| if *val { "1" } else { "0" })
        .collect::<String>();

    isize::from_str_radix(&s, 2).unwrap()
}

fn run(map: HashMap<String, bool>, vec: Vec<Gate>) -> HashMap<String, bool> {
    let mut vec = vec;
    let mut map = map;
    while vec.len() > 0 {
        vec.retain(|gate| {
            let Some(in1) = map.get(&gate.in1) else {
                return true;
            };
            let Some(in2) = map.get(&gate.in2) else {
                return true;
            };
            let res = match gate.oper {
                Operation::AND => in1 & in2,
                Operation::XOR => in1 ^ in2,
                Operation::OR => in1 | in2,
            };

            map.insert(gate.out.clone(), res);
            false
        });
    }
    return map;
}

fn werid_z(vec: &[Gate], n: usize) -> Option<(String, String)> {
    let x = format!("x{:02}", n);
    let zn = format!("z{:02}", n);

    let z = vec.iter().find(|gate| gate.out == zn)?;

    if let Some(other) = vec.iter().find(|gate| {
        [&z.in1, &z.in2].contains(&&gate.in1)
            && [&z.in1, &z.in2].contains(&&gate.in2)
            && gate.oper == Operation::XOR
    }) {
        if !other.in1.starts_with(['x', 'y']) && !other.in2.starts_with(['x', 'y']) {
            return Some((zn, other.out.clone()));
        }

        let other2 = vec.iter().find(|gate| {
            [&gate.in1, &gate.in2].contains(&&other.out) && gate.oper == Operation::XOR
        })?;
        return Some((zn, other2.out.clone()));
    }

    let x_gate = vec
        .iter()
        .find(|gate| (gate.in1 == x || gate.in2 == x) && gate.oper == Operation::XOR)?;
    let other = vec.iter().rev().find(|gate| {
        [&gate.in1, &gate.in2].contains(&&x_gate.out) && gate.oper == Operation::XOR
    })?;

    Some((zn, other.out.clone()))
}

// Idk if this works on all inputs. It worked for mine at least,
// maybe other bad patterns can be present which this function
// does not pick up. Idk
fn find_sus_gates(vec: &[Gate], n: usize) -> Option<(String, String)> {
    let x = format!("x{:02}", n);
    let zn = format!("z{:02}", n);

    let z = vec.iter().find(|gate| gate.out == zn)?;
    if z.oper != Operation::XOR {
        return werid_z(vec, n);
    }

    if let Some(other) = vec
        .iter()
        .find(|gate| (gate.in1 == x || gate.in2 == x) && gate.oper == Operation::XOR)
    {
        if ![&z.in1, &z.in2].contains(&&other.out) {
            let other2 = vec
                .iter()
                .find(|gate| (gate.in1 == x || gate.in2 == x) && gate.oper != Operation::XOR)?;
            return Some((other2.out.clone(), other.out.clone()));
        }
    }

    None
}

fn task_one(input: &[String]) -> isize {
    let (map, vec) = parse(input);
    let map = run(map, vec);
    convert(&map, 'z')
}

fn task_two(input: &[String]) -> String {
    let (_map, vec) = parse(input);

    let mut sus = Vec::new();
    for i in 2..45 {
        if let Some((x, y)) = find_sus_gates(&vec, i) {
            sus.push(x);
            sus.push(y);
        }
    }

    sus.sort();
    sus.join(",")
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
