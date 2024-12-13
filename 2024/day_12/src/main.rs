use std::collections::*;

type P = (isize, isize);
type HashSet<T> = fxhash::FxHashSet<T>;

const DIRS: [(isize, isize); 4] = [
    (1, 0),
    (0, 1),
    (0, -1),
    (-1, 0),
];

fn neighbors<T>(map: &[Vec<T>], (y, x): (isize, isize)) -> impl Iterator<Item = (isize, isize)> {
    let my = map.len() as isize;
    let mx = map[0].len() as isize;
    DIRS
        .iter()
        .map(move |dir| (y + dir.0, x + dir.1))
        .filter(move |(y, x)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
}

fn bfs(pos: (isize, isize), map: &[Vec<char>]) -> (HashSet<(isize, isize)>, usize) {
    let mut vec = VecDeque::new();
    let mut seen = HashSet::default();

    let plant = map[pos.0 as usize][pos.1 as usize];
    let mut perimiter = 0;

    vec.push_back(pos);
    seen.insert(pos);

    while  let Some(curr) = vec.pop_front() {
        let ns = neighbors(map, curr).filter(|(y, x)| map[*y as usize][*x as usize] == plant).collect::<Vec<_>>();
        perimiter += 4 - ns.len();
        for n in ns {
            if seen.insert(n) {
               vec.push_back(n);
            }
        }
    }

    (seen, perimiter)
}

fn count_sides(shape: &HashSet<(isize, isize)>) -> usize {
    let mut sum = 0;
    for dir in DIRS {
        let sides = shape
            .iter()
            .map(|pos| (pos.0 + dir.0, pos.1 + dir.1))
            .filter(|next| !shape.contains(next))
            .collect::<HashSet<_>>();

        let mut remove = HashSet::default();
        for side in &sides {
            let mut tmp = (side.0 + dir.1, side.1 + dir.0);
            while sides.contains(&tmp) {
                remove.insert(tmp);
                tmp = (tmp.0 + dir.1, tmp.1 + dir.0);
            }
        }
        sum += sides.len() - remove.len();
    }

    sum
}

fn solve<F>(input: &[String], mut f: F) -> usize
    where F: FnMut(P, &[Vec<char>]) -> (usize, HashSet<P>)
{
    let map: Vec<Vec<_>> = input.iter().map(|line| line.chars().collect()).collect();

    let mut sum = 0;
    let mut seen = HashSet::default();
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if seen.insert((y as isize, x as isize)) {
                let (s, area) = f((y as isize, x as isize), &map);
                sum += s;
                seen.extend(area);
            }
        }
    }

    sum
}

fn task_one(input: &[String]) -> usize {
    solve(input, |pos, map| {
        let (area, perimiter) = bfs(pos, &map);
        (area.len() * perimiter, area)
    })
}

fn task_two(input: &[String]) -> usize {
    solve(input, |pos, map|{
        let (area, _) = bfs(pos, &map);
        let sides = count_sides(&area);
        (area.len() * sides, area)
    })
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
