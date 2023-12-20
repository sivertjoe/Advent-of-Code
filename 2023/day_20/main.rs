use std::collections::*;

#[derive(Clone, Debug)]
enum Module {
    Broadcast,
    FlipFlop,
    Conjunction(HashMap<String, bool>),
}

fn parse(
    input: &[String],
) -> (
    HashMap<String, (Module, bool, Vec<String>)>,
    HashMap<String, bool>,
) {
    let mut map = HashMap::new();
    let mut state = HashMap::new();

    for line in input {
        let (fst, snd) = line.split_once(" -> ").unwrap();
        let send = snd.split(", ").map(|s| s.to_string()).collect::<Vec<_>>();

        let mut iter = fst.chars().peekable();
        let typ = *iter.peek().unwrap();
        let module = match typ {
            '%' => Module::FlipFlop,
            '&' => Module::Conjunction(HashMap::new()),
            _ => Module::Broadcast,
        };

        let name = if typ == '%' || typ == '&' {
            iter.skip(1).collect::<String>()
        } else {
            iter.collect::<String>()
        };

        state.insert(name.clone(), false);
        map.insert(name, (module, false, send));
    }

    (map, state)
}

fn press_button(map: &mut HashMap<String, (Module, bool, Vec<String>)>) -> (usize, usize) {
    send_pulse(map, "broadcaster", "button", false)
}
fn send_pulse(
    map: &mut HashMap<String, (Module, bool, Vec<String>)>,
    to: &str,
    from: &str,
    pulse: bool,
) -> (usize, usize) {
    println!("{} -{}> {}", from, pulse, to);
    if !map.contains_key(to) {
        return (0, 0);
    }
    let mut low = 0;
    let mut high = 0;

    if pulse {
        high += 1;
    } else {
        low += 1;
    }

    {
        let (module, state, _) = map.get_mut(to).unwrap();
        match module {
            Module::FlipFlop if pulse => {
                return (0, 0);
            }
            Module::FlipFlop if !pulse => {
                *state = !*state;
            }
            Module::Conjunction(ref mut map) => {
                map.insert(from.to_string(), pulse);
            }
            _ => {}
        }
    }
    let dests = map.get(to).unwrap().2.clone();
    for dst in dests {
        let p = {
            let (module, state, _) = map.get(to).unwrap();
            match module {
                Module::FlipFlop => *state,
                Module::Conjunction(st) => !st.values().all(|b| *b),
                Module::Broadcast => pulse,
            }
        };
        let (l, h) = send_pulse(map, &dst, to, p);
        low += l;
        high += h;
    }

    (low, high)
}

fn task_one(input: &[String]) -> usize {
    let (mut map, _) = parse(input);
    let mut low = 0;
    let mut high = 0;
    for _ in 0..1 {
        let (l, h) = press_button(&mut map);
        low += l;
        high += h;
    }
    low * high
}

fn task_two(input: &[String]) -> usize {
    unimplemented!()
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
