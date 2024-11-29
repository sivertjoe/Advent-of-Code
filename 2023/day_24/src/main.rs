fn parse(input: &[String]) -> Vec<Hail> {
    input
        .iter()
        .map(|line| {
            let (a, b) = line.split_once(" @ ").unwrap();
            let mut itera = a
                .split(", ")
                .map(|s| s.trim().parse::<isize>().unwrap() as f64);
            let mut iterb = b
                .split(", ")
                .map(|s| s.trim().parse::<isize>().unwrap() as f64);

            Hail {
                x: itera.next().unwrap(),
                y: itera.next().unwrap(),
                z: itera.next().unwrap(),

                vx: iterb.next().unwrap(),
                vy: iterb.next().unwrap(),
                vz: iterb.next().unwrap(),
            }
        })
        .collect::<Vec<_>>()
}

#[derive(Clone, Copy)]
struct Hail {
    x: f64,
    y: f64,
    z: f64,
    vx: f64,
    vy: f64,
    vz: f64,
}

impl Hail {
    fn to_line(self) -> Line {
        let m = self.vy / self.vx;
        let c = self.y - m * self.x;

        Line { m, c }
    }
}

struct Line {
    m: f64,
    c: f64,
}

fn line_cross(l1: &Line, l2: &Line) -> Option<(f64, f64)> {
    let tm = l1.m - l2.m;
    let tc = l2.c - l1.c;

    if l1.m == l2.m {
        None
    } else {
        let ix = tc / tm;
        let iy = l1.m * ix + l1.c;
        Some((ix, iy))
    }
}

fn task_one(input: &[String]) -> usize {
    let vec = parse(input);

    let mut count = 0;
    for i in 0..vec.len() {
        for j in i + 1..vec.len() {
            if let Some((x, y)) = line_cross(&vec[i].to_line(), &vec[j].to_line()) {
                if (vec[i].vx < 0.0 && x > vec[i].x) || (vec[i].vx > 0.0 && x < vec[i].x) {
                    continue;
                }
                if (vec[j].vx < 0.0 && x > vec[j].x) || (vec[j].vx > 0.0 && x < vec[j].x) {
                    continue;
                }
                const MIN: f64 = 200000000000000.0;
                const MAX: f64 = 400000000000000.0;
                if [x, y].into_iter().all(|p| (MIN..=MAX).contains(&p)) {
                    count += 1;
                }
            }
        }
    }

    count
}

use z3::{ast::Ast, ast::Int, Config, Context, Solver};

fn task_two(input: &[String]) -> usize {
    let hail = parse(input);

    let context = Context::new(&Config::new());
    let solver = Solver::new(&context);

    let x = Int::new_const(&context, "x");
    let y = Int::new_const(&context, "y");
    let z = Int::new_const(&context, "z");

    let vx = Int::new_const(&context, "vx");
    let vy = Int::new_const(&context, "vy");
    let vz = Int::new_const(&context, "vz");

    for h in hail {
        let tn = Int::fresh_const(&context, "t");

        for (pi, vpi, p, v) in [
            (h.x, h.vx, &x, &vx),
            (h.y, h.vy, &y, &vy),
            (h.z, h.vz, &z, &vz),
        ] {
            let pn = Int::from_i64(&context, pi as i64);
            let vpn = Int::from_i64(&context, vpi as i64);
            solver.assert(&(pn + vpn * &tn)._eq(&(p + v * &tn)));
        }
    }

    solver.check();
    let model = solver.get_model().unwrap();
    [&x, &y, &z]
        .into_iter()
        .map(|i| model.get_const_interp(i).unwrap().as_i64().unwrap())
        .sum::<i64>() as usize
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
