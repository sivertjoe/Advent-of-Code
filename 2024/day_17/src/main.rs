const A: usize = 0;
const B: usize = 1;
const C: usize = 2;

fn parse(input: &[String]) -> (Vec<usize>, Vec<usize>) {
    let mut iter = input.split(|line| line.is_empty());

    let regs = iter.next().unwrap();
    let regs = regs
        .iter()
        .map(|line| line.split(": ").nth(1).unwrap().parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    let ins = iter.next().unwrap();
    let ins = ins[0]
        .split(": ")
        .nth(1)
        .unwrap()
        .split(',')
        .map(|n| n.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    (regs, ins)
}

fn get(regs: &[usize], op: usize) -> usize {
    match op {
        0..=3 => op,
        4 => regs[A],
        5 => regs[B],
        6 => regs[C],
        7.. => unreachable!(),
    }
}

fn run(regs: &mut [usize], ins: &[usize]) -> (usize, bool) {
    let mut ip = 0;

    let mut out = 0;
    loop {
        let curr = ins[ip];
        let oper = ins[ip + 1];
        ip += 2;
        match curr {
            0 => regs[A] /= 2usize.pow(get(&regs, oper) as _),
            1 => regs[B] ^= oper,
            2 => regs[B] = get(&regs, oper) % 8,
            3 => return (out, regs[A] != 0),
            4 => regs[B] ^= regs[C],
            5 => out = get(&regs, oper) % 8,
            6 => regs[B] = regs[A] / 2usize.pow(get(&regs, oper) as _),
            7 => regs[C] = regs[A] / 2usize.pow(get(&regs, oper) as _),
            _ => unreachable!(),
        }
    }
}

fn rec<F>(n: usize, program: &[usize], cycle: &mut F) -> Option<usize>
where
    F: FnMut(usize) -> usize,
{
    match program {
        [] => Some(n),
        [start @ .., last] => (0..=7).find_map(|i| {
            let next = n * 8 + i;
            if cycle(next) % 8 == *last {
                rec(next, start, cycle)
            } else {
                None
            }
        }),
    }
}

fn task_one(input: &[String]) -> String {
    let (mut regs, ins) = parse(input);
    let mut outs = Vec::new();

    loop {
        let (out, cont) = run(&mut regs, &ins);
        outs.push(out);
        if !cont {
            break;
        }
    }
    outs.into_iter()
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join(",")
}

fn task_two(input: &[String]) -> usize {
    let (mut regs, ins) = parse(input);

    let mut cycle = |a: usize| {
        regs[A] = a;
        regs[B] = 0;
        regs[C] = 0;
        let (out, _) = run(&mut regs, &ins);
        out
    };

    rec(0, &ins, &mut cycle).unwrap()
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
