use std::cmp::Reverse;
use std::collections::*;

fn parse(input: &[String]) -> Vec<Vec<usize>> {
    input
        .iter()
        .map(|line| {
            line.chars()
                .map(|b| b.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

#[derive(Hash, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
enum Dir {
    Right,
    Left,
    Up,
    Down,
}

fn neighbors<const MIN: usize, const MAX: usize>(
    vec: &[Vec<usize>],
    idx: (usize, usize),
    (gdir, count): (Dir, usize),
) -> impl Iterator<Item = ((usize, usize), (Dir, usize))> {
    let my = vec.len() as isize;
    let mx = vec[0].len() as isize;
    let y = idx.0 as isize;
    let x = idx.1 as isize;

    let in_bounds = move |y: isize, x: isize| x >= 0 && x < mx && y >= 0 && y < my;
    let not_reverse = move |new: Dir| {
        !((new == Dir::Up && gdir == Dir::Down)
            || (new == Dir::Down && gdir == Dir::Up)
            || (new == Dir::Left && gdir == Dir::Right)
            || (new == Dir::Right && gdir == Dir::Left))
    };

    let allowed_steps =
        move |new: Dir| !(new == gdir && count == (MAX - 1) || new != gdir && count < (MIN - 1));

    let get_dir = move |y: isize, x: isize| {
        if y == idx.0 as isize {
            if x > idx.1 as isize {
                Dir::Right
            } else {
                Dir::Left
            }
        } else if y > idx.0 as isize {
            Dir::Down
        } else {
            Dir::Up
        }
    };

    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        .into_iter()
        .filter_map(move |(y, x)| {
            let dir = get_dir(y, x);
            let ncount = if dir == gdir { count + 1 } else { 0 };
            (in_bounds(y, x) && not_reverse(dir) && allowed_steps(dir))
                .then(|| ((y as usize, x as usize), (dir, ncount)))
        })
}

fn least_heat_loss<const MIN: usize, const MAX: usize>(input: &[String]) -> usize {
    let vec = parse(input);
    let mut seen = HashSet::new();
    let start = (0, 0);
    seen.insert((start, (Dir::Right, 0)));

    let end = (vec.len() - 1, vec[0].len() - 1);

    let mut stack = BinaryHeap::new();
    stack.push((Reverse(0), start, (Dir::Right, 0)));
    stack.push((Reverse(0), start, (Dir::Down, 0)));

    while let Some((cost, p, info)) = stack.pop() {
        if p == end && info.1 >= (MIN - 1) {
            return cost.0;
        }

        for (ns, ninfo) in neighbors::<MIN, MAX>(&vec, p, info) {
            let new_cost = cost.0 + vec[ns.0][ns.1];
            if seen.insert((ns, ninfo)) {
                stack.push((Reverse(new_cost), ns, ninfo));
            }
        }
    }

    unreachable!()
}

fn task_one(input: &[String]) -> usize {
    least_heat_loss::<1, 3>(input)
}

fn task_two(input: &[String]) -> usize {
    least_heat_loss::<4, 10>(input)
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
