use std::collections::*;

fn _solve2(
    recs_i: usize,
    recs: &mut [u8],
    nums_i: usize,
    nums: &[usize],
    prev_hash: usize,
    //dp: &mut HashMap<(recs_i, nums_i, prev_hash), usize>
    dp: &mut HashMap<(usize, usize, usize, u8), usize>
) -> usize{
    let mut loc = 0;
    if recs_i >= recs.len() {
        if prev_hash > 0 {
            if nums_i == nums.len() - 1 && prev_hash == nums[nums_i] {
                return 1;
            }
        } else { 
            if nums_i >= nums.len()  {
                return 1;
            } else {

            }
        }
        //return true;
        return 0;
    }
    if nums_i >= nums.len() {
        if recs[recs_i..].iter().any(|r| *r == b'#') {
            return 0;
        } else {
            return 1;
        }
    }

    if prev_hash > 0 && prev_hash > nums[nums_i] {
        return 0;
    }
    let elem = recs[recs_i];
    match elem {
        b'?' => {
            recs[recs_i] = b'#';

            let k1 = (recs_i, nums_i, prev_hash, b'#');
            let k2 = (recs_i, nums_i, prev_hash, b'.');

            if !dp.contains_key(&k1)
            {
                let r = _solve2(recs_i, recs, nums_i, nums, prev_hash, dp);
                dp.insert(k1, r);
            }
            loc += *dp.get(&k1).unwrap();

            recs[recs_i] = b'.';
            if !dp.contains_key(&k2)
            {
                let r = _solve2(recs_i, recs, nums_i, nums, prev_hash, dp);
                dp.insert(k2, r);
            }
            loc += *dp.get(&k2).unwrap();

            recs[recs_i] = b'?';
        }
        b'#' => {
            loc += _solve2(recs_i + 1, recs, nums_i, nums, prev_hash + 1, dp);
        }
        b'.' => {
            if prev_hash > 0 && prev_hash == nums[nums_i] {
                loc += _solve2(recs_i + 1, recs, nums_i + 1, nums, 0, dp);
            } else if prev_hash == 0 {
                loc += _solve2(recs_i + 1, recs, nums_i, nums, 0, dp);
            }
        }
        _ => unreachable!()
    }
    loc
}

fn parse(input: &[String]) -> Vec<(Vec<u8>, Vec<usize>)> {
    input
        .iter()
        .map(|line| {
            let (fst, snd) = line.split_once(' ').unwrap();
            let fst = fst
                .bytes()
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

    for (recs, nums) in data {
        let local = _solve2(0, &mut recs.clone(), 0, &nums, 0, &mut HashMap::new());
        count += local;
    }
    count
}

fn transform((recs, nums): (Vec<u8>, Vec<usize>)) -> (Vec<u8>, Vec<usize>) {
    let mut nrecs = Vec::new();
    let mut nnums = Vec::new();

    for _ in 0..5 {
        nrecs.extend(recs.clone());
        nrecs.push(b'?');
        nnums.extend(nums.clone());
    }
    let _ = nrecs.pop();

    (nrecs, nnums)
}

fn task_two(input: &[String]) -> usize {
    let data = parse(input).into_iter().map(transform).collect::<Vec<_>>();
    let mut count = 0;


   for (recs, nums) in data {
        let local = _solve2(0, &mut recs.clone(), 0, &nums,  0, &mut HashMap::new());
        count += local;
    }
    count
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
