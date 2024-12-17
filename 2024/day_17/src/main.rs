use std::collections::*;

fn parse(input: &[String]) -> (Vec<isize>, Vec<usize>) {
    let mut iter = input.split(|line| line.is_empty());

    let regs = iter.next().unwrap();
    let regs = regs
        .iter()
        .map(|line| line.split(": ").nth(1).unwrap().parse::<isize>().unwrap())
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

fn get_operand(regs: &[isize], op: usize) -> isize {
    match op {
        0..=3 => op as isize,
        4 => regs[0],
        5 => regs[1],
        6 => regs[2],
        7 => todo!(),
        _ => unreachable!(),
    }
}

fn task_one(input: &[String]) -> String {
    let (mut regs, ins) = parse(input);
    let mut out = Vec::new();

    let mut ip = 0;

    loop {
        if ip >= ins.len() {
            break;
        }
        match ins[ip] {
            0 => {
                let oper = get_operand(&regs, ins[ip + 1]);
                // adv
                let numerator = regs[0];
                let denominator = 2isize.pow(oper as _);

                let res = numerator / denominator;
                regs[0] = res;
                ip += 2;
            }

            1 => {
                // bxl
                regs[1] = regs[1] ^ ins[ip + 1] as isize;
                ip += 2;
            }

            2 => {
                // bst
                let oper = get_operand(&regs, ins[ip + 1]);
                regs[1] = oper % 8;
                ip += 2;
            }
            3 => {
                // jnz
                if regs[0] == 0 {
                    ip += 2;
                    // do nothing
                } else {
                    assert!(ins[ip + 1] <= 3);
                    ip = ins[ip + 1] as usize;
                }
            }
            4 => {
                // bxc
                regs[1] = regs[1] ^ regs[2];
                ip += 2;
            }
            5 => {
                let oper = get_operand(&regs, ins[ip + 1]);
                let ch = oper % 8;
                out.push((ch as u8).to_string());
                ip += 2;
            }
            6 => {
                let oper = get_operand(&regs, ins[ip + 1]);
                let numerator = regs[0];
                let denominator = 2isize.pow(oper as _);

                let res = numerator / denominator;
                regs[1] = res;
                ip += 2;
            }
            7 => {
                let oper = get_operand(&regs, ins[ip + 1]);
                let numerator = regs[0];
                let denominator = 2isize.pow(oper as _);

                let res = numerator / denominator;
                regs[2] = res;
                ip += 2;
            }
            _ => unreachable!(),
        }
    }
    println!("{:?}", regs);
    out.join("")
}
// 150520135 x
// 150520135 ?

fn task_two(input: &[String]) -> usize {
    unimplemented!()
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
