use std::{cmp::Reverse, collections::*};

type Diagram = Vec<bool>;
type Wiring = Vec<Vec<usize>>;
type Schematics = Vec<usize>;

fn parse(input: &[String]) -> Vec<(Diagram, Wiring, Schematics)> {
    let mut to_ret = Vec::new();
    for line in input {
        let (diagram, rest) = line.split_once(" ").unwrap();
        let diagram = diagram
            .chars()
            .flat_map(|ch| {
                if ch == '[' || ch == ']' {
                    None
                } else if ch == '.' {
                    Some(false)
                } else {
                    Some(true)
                }
            })
            .collect::<Vec<_>>();

        let (rest, schemantics) = rest.rsplit_once(" ").unwrap();
        let schemantics = schemantics[1..schemantics.len() - 1]
            .split(",")
            .map(|n| n.parse::<usize>().unwrap())
            .collect::<Vec<_>>();

        let wiring = rest
            .replace('(', "")
            .replace(')', "")
            .split_whitespace()
            .map(|elem| {
                elem.split(',')
                    .map(|num| num.parse::<usize>().unwrap())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        to_ret.push((diagram, wiring, schemantics));
    }
    to_ret
}

fn apply(state: &mut Vec<bool>, wiring: &[usize]) {
    for w in wiring {
        state[*w] = !state[*w];
    }
}

fn bfs(diagram: Diagram, wiring: Wiring) -> usize {
    let mut initial = vec![false; diagram.len()];

    let mut seen: HashMap<Vec<bool>, usize> = HashMap::new();
    let mut vec = VecDeque::new();
    vec.push_back((0, initial));

    while let Some((steps, state)) = vec.pop_front() {
        if state == diagram {
            return steps;
        }
        seen.insert(state.clone(), steps);

        for w in wiring.iter() {
            let mut new_state = state.clone();
            let new_steps = steps + 1;
            apply(&mut new_state, w);
            if let Some(old) = seen.get(&new_state)
                && *old > new_steps
            {
                vec.push_back((new_steps, new_state));
            } else if !seen.contains_key(&new_state) {
                vec.push_back((new_steps, new_state));
            }
        }
    }

    todo!()
}
fn apply2(jolts: &mut Vec<usize>, wiring: &[usize]) {
    for w in wiring {
        jolts[*w] += 1;
    }
}

fn confirm_jolts(local: &[usize], jolts: &[usize]) -> bool {
    local
        .iter()
        .copied()
        .zip(jolts.iter().copied())
        .all(|(local, jolt)| local <= jolt)
}

fn bfs2(wiring: Wiring, jolts: Schematics) -> usize {
    let mut initial_jolts = vec![0; jolts.len()];

    let mut seen: HashMap<Vec<usize>, usize> = HashMap::new();
    seen.insert(initial_jolts.clone(), 0);
    let mut vec = BinaryHeap::new();
    vec.push((Reverse(0), initial_jolts));

    while let Some((steps, local_jolts)) = vec.pop() {
        if local_jolts == jolts {
            return steps.0;
        }

        seen.insert(local_jolts.clone(), steps.0);

        for w in wiring.iter() {
            let mut new_jolts = local_jolts.clone();
            let new_steps = Reverse(steps.0 + 1);
            apply2(&mut new_jolts, w);

            if let Some(num) = seen.get(&new_jolts)
                && *num > new_steps.0
                && confirm_jolts(&new_jolts, &jolts)
            {
                vec.push((new_steps, new_jolts.clone()));
            } else if !seen.contains_key(&new_jolts) && confirm_jolts(&new_jolts, &jolts) {
                vec.push((new_steps, new_jolts));
            }
        }
    }

    todo!()
}

fn task_one(input: &[String]) -> usize {
    let machines = parse(input);
    let mut sum = 0;
    for (diagram, wiring, _) in machines {
        let local = bfs(diagram, wiring);
        sum += local;
    }
    sum
}

fn task_two(input: &[String]) -> usize {
    let machines = parse(input);
    let mut sum = 0;
    for (diagram, wiring, jolts) in machines {
        println!("{:?} {:?} {:?}", diagram, wiring, jolts);
        let local = bfs2(wiring, jolts);
        println!("{}", local);
        sum += local;
    }
    sum
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
