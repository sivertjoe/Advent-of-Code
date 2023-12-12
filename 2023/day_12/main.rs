use std::collections::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Record {
    Operational,
    Damaged,
    Unknown,
}

fn verify(curr: Vec<Record>, nums: &[usize]) -> bool {
    let mut i = 0;
    for num in nums {
        while i < curr.len() {
            if curr[i] == Record::Damaged {
                break;
            }
            i += 1;
        }
        let dcount = curr[i..]
            .iter()
            .take_while(|rec| **rec == Record::Damaged)
            .count();

        if i + num > curr.len() {
            return false;
        }

        if !curr[i..i + num].iter().all(|rec| *rec == Record::Damaged) || *num != dcount {
            return false;
        }
        i += num;
    }
    let s1 = curr.iter().filter(|rec| **rec == Record::Damaged).count();
    let s2 = nums.iter().copied().sum();

    s1 == s2
}

fn _print(recs: &[Record]) {
    let mut builder = String::new();
    for rec in recs {
        let cha = match rec {
            Record::Unknown => '?',
            Record::Damaged => '#',
            _ => '.',
        };
        builder.push(cha);
    }
    println!("{}", builder);
}

fn _solve(i: usize, recs: &[Record], nums: &[usize], curr: Vec<Record>, count: &mut usize) {
    if i >= recs.len() {
        if verify(curr.clone(), nums) {
            *count += 1;
        }
        return;
    }
    let mut curr = curr;

    let elem = recs[i];
    if elem == Record::Unknown {
        let mut curr2 = curr.clone();
        curr2.push(Record::Operational);
        _solve(i + 1, recs, nums, curr2, count);

        curr.push(Record::Damaged);
        _solve(i + 1, recs, nums, curr, count);
    } else {
        curr.push(elem);
        _solve(i + 1, recs, nums, curr, count);
    }
}

/*fn solve(recs: &[Record], nums: &[usize]) -> usize {
    let mut vecs = Vec::new();
    _solve(&recs, &nums, &mut vecs);
    0
}*/

fn parse(input: &[String]) -> Vec<(Vec<Record>, Vec<usize>)> {
    input
        .iter()
        .map(|line| {
            let (fst, snd) = line.split_once(' ').unwrap();
            let fst = fst
                .bytes()
                .map(|b| match b {
                    b'#' => Record::Damaged,
                    b'.' => Record::Operational,
                    b'?' => Record::Unknown,
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            let snd = snd
                .split(',')
                .map(|d| d.parse::<usize>().unwrap())
                .collect::<Vec<_>>();

            (fst, snd)
        })
        .collect()
}

fn task_one(input: &[String]) -> usize {
    let data = parse(input);
    let mut count = 0;

    //_solve(0, &data[5].0, &data[5].1, vec![], &mut count);

    for (recs, nums) in data {
        let mut local = 0;
        _solve(0, &recs, &nums, vec![], &mut local);
        _print(&recs);
        println!("{}\n", local);
        count += local;
    }
    count
}

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
