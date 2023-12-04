struct Number(usize, usize);
struct Symbol(char);

type NumbersMatrix = Vec<Vec<(Number, usize)>>;
type SymbolList = Vec<(Symbol, usize, usize)>;

fn parse_schematic(input: &[String]) -> (NumbersMatrix, SymbolList) {
    let mut nums = Vec::with_capacity(140);
    let mut syms = Vec::new();
    for (y, line) in input.iter().enumerate() {
        let mut lnums = Vec::with_capacity(10);

        let mut number = String::with_capacity(3);
        let mut index = 0;
        for (x, ch) in line.bytes().enumerate() {
            if !ch.is_ascii_digit() && ch != b'.' {
                syms.push((Symbol(ch as char), x, y));
            }
            if ch.is_ascii_digit() {
                if number.is_empty() {
                    index = x;
                }
                number.push(ch as char);
            } else if !number.is_empty() {
                let g = number.parse().unwrap();
                lnums.push((Number(g, number.len() - 1), index));
                number.clear();
            }
        }

        if !number.is_empty() {
            let g = number.parse().unwrap();
            lnums.push((Number(g, number.len() - 1), index));
        }

        nums.push(lnums);
    }

    (nums, syms)
}

fn find(x: usize, y: usize, numbers: &[Vec<(Number, usize)>]) -> Vec<usize> {
    let mut vec = Vec::with_capacity(4);
    for y in [
        y.saturating_sub(1),
        y,
        std::cmp::min(numbers.len() - 1, y + 1),
    ] {
        for (num, _x) in &numbers[y] {
            let x0 = *_x;
            let x1 = *_x + num.1;

            if (x0..=x1).any(|nx| nx.abs_diff(x) <= 1) {
                vec.push(num.0);
            }
        }
    }
    vec
}

fn task_one(input: &[String]) -> usize {
    let (numbers, symbols) = parse_schematic(input);

    symbols
        .into_iter()
        .flat_map(|(_sym, x, y)| find(x, y, &numbers).into_iter())
        .sum()
}

fn task_two(input: &[String]) -> usize {
    let (numbers, symbols) = parse_schematic(input);

    let mut sum = 0;
    for (sym, x, y) in symbols {
        if sym.0 == '*' {
            let nums = find(x, y, &numbers);
            if nums.len() == 2 {
                sum += nums[0] * nums[1]
            }
        }
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
