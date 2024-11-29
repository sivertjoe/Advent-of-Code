fn map_simple(line: &str) -> (isize, (isize, isize)) {
    let mut iter = line.split_ascii_whitespace();
    let dir = iter.next().unwrap();
    let num = iter.next().unwrap().parse::<isize>().unwrap();

    let d = match dir {
        "U" => (-1, 0),
        "D" => (1, 0),
        "R" => (0, 1),
        "L" => (0, -1),
        _ => unreachable!(),
    };
    (num, d)
}

fn map_complex(line: &str) -> (isize, (isize, isize)) {
    let iter = line.split_ascii_whitespace();
    let hex = iter.last().unwrap();

    let num = &hex[2..7];
    let dir = &hex[7..8];

    let num = isize::from_str_radix(num, 16).unwrap();

    let d = match dir.trim() {
        "3" => (-1, 0),
        "1" => (1, 0),
        "0" => (0, 1),
        "2" => (0, -1),
        _ => unreachable!(),
    };
    (num, d)
}

fn poligon_area(verts: &[(isize, isize)]) -> isize {
    let num = verts.len();
    let mut sum1 = 0;
    let mut sum2 = 0;

    for i in 0..num - 1 {
        sum1 += verts[i].0 * verts[i + 1].1;
        sum2 += verts[i].1 * verts[i + 1].0;
    }

    sum1 += verts[num - 1].0 * verts[0].1;
    sum2 += verts[num - 1].1 * verts[0].0;

    (sum1 - sum2).abs() / 2
}

fn lagoon_area<F>(input: &[String], f: F) -> usize
where
    F: Fn(&str) -> (isize, (isize, isize)),
{
    let mut vec = Vec::new();
    let mut curr = (0, 0);

    let mut perim = 0;
    vec.push(curr);

    for line in input.iter() {
        let (num, d) = f(line);
        curr.0 += d.0 * num;
        curr.1 += d.1 * num;

        perim += (d.0 * num + d.1 * num).abs();

        vec.push(curr);
    }
    ((perim / 2 + 1) + poligon_area(&vec)) as _
}

fn task_one(input: &[String]) -> usize {
    lagoon_area(input, map_simple)
}
fn task_two(input: &[String]) -> usize {
    lagoon_area(input, map_complex)
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
