type HashSet<V> = fxhash::FxHashSet<V>;

fn rotate(n: (isize, isize)) -> (isize, isize) {
    match n {
        (-1, 0) => (0, 1),
        (0, 1) => (1, 0),
        (1, 0) => (0, -1),
        (0, -1) => (-1, 0),
        _ => unreachable!(),
    }
}

fn next(pos: (usize, usize), inc: (isize, isize), vec: &[Vec<u8>]) -> Option<(usize, usize)> {
    let Some(y) = pos.0.checked_add_signed(inc.0) else {
        return None;
    };
    let Some(x) = pos.1.checked_add_signed(inc.1) else {
        return None;
    };

    (y < vec.len() && x < vec[0].len()).then_some((y, x))
}

fn explore_map(
    vec: &[Vec<u8>],
    pos: (usize, usize),
    inc: (isize, isize),
    brick_pos: Option<(usize, usize)>,
) -> Option<usize> {
    let mut seen = HashSet::default();

    let mut pos = pos;
    let mut inc = inc;

    seen.insert((pos, inc));

    while let Some(next @ (y, x)) = next(pos, inc, vec) {
        match vec[y][x] {
            b'#' => {
                inc = rotate(inc);
            }
            _ch if Some(next) == brick_pos => {
                inc = rotate(inc);
            }
            b'.' => {
                // loop
                if !seen.insert((next, inc)) {
                    return None;
                }
                pos = next;
            }

            _ => unreachable!(),
        }
    }

    Some(
        seen.into_iter()
            .map(|(pos, _inc)| pos)
            .collect::<HashSet<_>>()
            .len(),
    )
}

fn task_one(input: &[String]) -> usize {
    let mut vec: Vec<Vec<u8>> = input.iter().map(|line| line.as_bytes().to_vec()).collect();

    let pos = vec.iter().flatten().position(|ch| *ch == b'^').unwrap();

    let pos @ (y, x) = (pos / vec.len(), pos % vec.len());
    let inc = (-1, 0);
    vec[y][x] = b'.';

    explore_map(&vec, pos, inc, None).unwrap()
}

fn task_two(input: &[String]) -> usize {
    use rayon::prelude::*;
    let mut vec: Vec<Vec<u8>> = input.iter().map(|line| line.as_bytes().to_vec()).collect();

    let pos = vec.iter().flatten().position(|ch| *ch == b'^').unwrap();

    let pos @ (y, x) = (pos / vec.len(), pos % vec.len());
    let inc = (-1, 0);
    vec[y][x] = b'.';

    let tiles = vec
        .iter()
        .flatten()
        .enumerate()
        .flat_map(|(pos, ch)| (*ch == b'.').then_some((pos / vec.len(), pos % vec.len())))
        .collect::<Vec<_>>();

    tiles
        .into_par_iter()
        .filter(|brick| explore_map(&vec, pos, inc, Some(*brick)).is_none())
        .count()
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
