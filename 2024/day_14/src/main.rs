use std::collections::*;

type P = (isize, isize);

fn parse(input: &[String]) -> Vec<(P, P)> {
    input
        .iter()
        .map(|line| {
            let line = line.replace("p=", "").replace("v=", "");

            let mut split = line.split_whitespace();
            let mut fst = split
                .next()
                .unwrap()
                .split(',')
                .map(|n| n.parse::<isize>().unwrap());
            let mut snd = split
                .next()
                .unwrap()
                .split(',')
                .map(|n| n.parse::<isize>().unwrap());

            (
                (fst.next().unwrap(), fst.next().unwrap()),
                (snd.next().unwrap(), snd.next().unwrap()),
            )
        })
        .collect::<Vec<_>>()
}

fn print_map(map: &HashMap<P, Vec<P>>) {
    for y in 0..H {
        for x in 0..W {
            if let Some(v) = map.get(&(y as isize, x as isize)) {
                print!("{}", v.len());
            } else {
                print!(".");
            }
        }
        println!();
    }
    println!();
}

const W: usize = 101;
const H: usize = 103;
// const W: usize = 11;
// const H: usize = 7;

fn task_one(input: &[String]) -> usize {
    let vec = parse(input);

    let mut map: HashMap<P, Vec<P>> = HashMap::new();

    for robot in vec {
        map.entry((robot.0 .1, robot.0 .0))
            .or_default()
            .push((robot.1 .1, robot.1 .0));
    }

    for _ in 0..100 {
        map = update(map);
    }

    let qs = [
        ((0, 0), (H / 2, W / 2)),             // TOP LEFT
        ((0, (W / 2) + 1), (H / 2, W)),       // TOP RIGHT
        (((H / 2) + 1, 0), (H, W / 2)),       // BOT LEFT
        (((H / 2) + 1, (W / 2) + 1), (H, W)), // BOT RIGHT
    ];
    let mut prod = 1;
    for q in qs {
        let mut sum = 0;
        for y in q.0 .0..q.1 .0 {
            for x in q.0 .1..q.1 .1 {
                if let Some(v) = map.get(&(y as _, x as _)) {
                    sum += v.len();
                }
            }
        }
        prod *= sum;
    }

    prod
}

fn update(map: HashMap<P, Vec<P>>) -> HashMap<P, Vec<P>> {
    let mut new: HashMap<P, Vec<P>> = HashMap::with_capacity(map.len());

    for (pos, robots) in map {
        for robot_vel in robots {
            let new_pos = (
                (pos.0 + robot_vel.0).rem_euclid(H as isize),
                (pos.1 + robot_vel.1).rem_euclid(W as isize),
            );
            new.entry(new_pos).or_default().push(robot_vel);
        }
    }

    new
}

fn task_two(input: &[String]) -> usize {
    let vec = parse(input);

    let mut map: HashMap<P, Vec<P>> = HashMap::new();

    for robot in vec {
        map.entry((robot.0 .1, robot.0 .0))
            .or_default()
            .push((robot.1 .1, robot.1 .0));
    }

    for i in 1..=6668 {
        map = update(map);
    }

    6668
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
