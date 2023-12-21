use std::collections::*;

#[derive(Clone, Debug)]
enum Module {
    Broadcast,
    FlipFlop,
    Conjunction(HashMap<String, bool>),
}

type Map = HashMap<String, (Module, bool, Vec<String>)>;

fn parse(input: &[String]) -> Map {
    let mut map = HashMap::new();

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

        map.insert(name, (module, false, send));
    }

    let mut temp = Vec::new();
    for (name, val) in map.iter() {
        if matches!(val.0, Module::Conjunction(_)) {
            let inputs = {
                map.iter()
                    .map(|state| state.clone())
                    .filter_map(|(n, value)| value.2.contains(name).then_some(n.clone()))
                    .collect::<Vec<_>>()
            };
            temp.push((name.clone(), inputs));
        }
    }

    for (name, input) in temp {
        for i in input {
            match map.get_mut(&name).unwrap().0 {
                Module::Conjunction(ref mut map) => {
                    map.insert(i, false);
                }
                _ => {}
            }
        }
    }

    map
}

fn handle_pulse(
    map: &mut Map,
    from: String,
    to: String,
    pulse: bool,
) -> Option<(bool, Vec<String>)> {
    if !map.contains_key(&to) {
        return None;
    }
    let (module, state, dests) = map.get_mut(&to).unwrap();
    match module {
        Module::FlipFlop => {
            if pulse {
                None
            } else {
                *state = !*state;
                Some((*state, dests.clone()))
            }
        }
        Module::Conjunction(ref mut map) => {
            map.insert(from, pulse);
            Some((!map.values().all(|b| *b), dests.clone()))
        }
        Module::Broadcast => Some((pulse, dests.clone())),
    }
}

fn press_button(
    map: &mut Map,
    vecs: &[String],
    temp: &mut HashMap<String, usize>,
    i: usize,
) -> (usize, usize) {
    let mut stack = VecDeque::new();
    stack.push_back(("button".to_string(), "broadcaster".to_string(), false));
    let mut high = 0;
    let mut low = 1;

    while let Some((from, to, pulse)) = stack.pop_front() {
        if let Some((pulse, dsts)) = handle_pulse(map, from, to.clone(), pulse) {
            // For part 2
            if vecs.contains(&to) && pulse && !temp.contains_key(&to) {
                temp.insert(to.clone(), i);
            }
            for dst in dsts {
                stack.push_back((to.clone(), dst, pulse));
                if pulse {
                    high += 1;
                } else {
                    low += 1;
                }
            }
        }
    }
    (low, high)
}

fn get_inputs_for(map: &Map, module: String) -> Vec<String> {
    let mut vec = Vec::new();

    for (name, (typ, _, dsts)) in map {
        if matches!(typ, Module::Conjunction(_)) && dsts.contains(&module) {
            vec.push(name.clone());
        }
    }

    vec
}

fn get_rx_source(map: &Map) -> String {
    let rx = "rx".to_string();
    for (name, (.., dsts)) in map {
        if dsts.contains(&rx) {
            return name.clone();
        }
    }
    unreachable!()
}

fn task_one(input: &[String]) -> usize {
    let mut map = parse(input);
    let mut low = 0;
    let mut high = 0;
    for _ in 0..1000 {
        let (l, h) = press_button(&mut map, &[], &mut HashMap::new(), 0);
        low += l;
        high += h;
    }
    low * high
}

fn task_two(input: &[String]) -> usize {
    let mut map = parse(input);
    let inputs = get_inputs_for(&map, get_rx_source(&map));
    let mut cycles = HashMap::new();
    let mut i = 1;

    while cycles.len() != inputs.len() {
        press_button(&mut map, &inputs, &mut cycles, i);
        i += 1;
    }
    cycles.into_values().product()
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
