use std::collections::*;

fn generate(input: &[String]) -> Vec<isize> {
    let mut iter = input[0].chars();
    let mut vec = Vec::new();

    let mut i = 0;
    loop {
        let Some(n) = iter.next() else { break };
        let n = n.to_digit(10).unwrap();
        vec.append(&mut (0..n).map(|_| i).collect());
        i += 1;
        let Some(n) = iter.next() else { break };
        let n = n.to_digit(10).unwrap();
        vec.append(&mut (0..n).map(|_| -1).collect());
    }

    vec
}

fn checksum(vec: &[isize]) -> usize {
    vec.into_iter()
        .enumerate()
        .filter(|(_i, n)| **n != -1)
        .map(|(i, n)| i * *n as usize)
        .sum()
}

fn task_one(input: &[String]) -> usize {
    let mut vec = generate(input);

    let mut i = 0;
    let mut j = vec.len() - 1;
    loop {
        while i < vec.len() {
            if vec[i] == -1 {
                break;
            }
            i += 1;
        }
        while j >= 0 {
            if vec[j] != -1 {
                break;
            }
            j -= 1;
        }

        if i >= j {
            break;
        }

        let temp = vec[i];
        vec[i] = vec[j];
        vec[j] = temp;
    }
    checksum(&vec)
}

fn find_section(vec: &[isize]) -> Vec<(usize, usize)> {
    let mut sec = Vec::new();

    let mut i = 0;
    let mut j = 0;

    loop {
        while j < vec.len() && vec[j] == vec[i] {
            j += 1;
        }
        sec.push((i, j - 1));
        i = j;

        if j == vec.len() {
            break;
        }

        while vec[i] == -1 {
            i += 1;
        }
        j = i;
    }

    sec
}

fn find_empty_sections(vec: &[isize]) -> Vec<(usize, usize)> {
    let mut init = (0, 0);
    let mut empty = vec![];

    loop {
        let next = find_next_empty_section(init, &vec);
        if next.0 > next.1 {
            break;
        }
        empty.push(next);
        init = next;
    }
    empty
}

fn find_next_empty_section((i, ii): (usize, usize), vec: &[isize]) -> (usize, usize) {
    let mut ni = ii;
    // skip leftover -1
    while ni < vec.len() && vec[ni] == -1 {
        ni += 1;
    }

    while ni < vec.len() && vec[ni] != -1 {
        ni += 1;
    }

    let mut nii = ni;
    while nii < vec.len() && vec[nii] == -1 {
        nii += 1;
    }

    nii -= 1;
    (ni, nii)
}

fn print(vec: &[isize]) {
    /*for i in 0..vec.len() {
        print!("{}", i % 10);
    }*/
    println!();
    for ch in vec {
        if *ch == -1 {
            print!(".");
        } else {
            let ch = (b'0' + (*ch as u8)) as char;
            print!("{ch}");
        }
    }
    println!();
}

fn task_two(input: &[String]) -> usize {
    let mut vec = generate(input);

    let mut empty = find_empty_sections(&vec);
    let mut secs = find_section(&vec);

    while let Some(file) = secs.pop() {
        if let Some(index) = empty.iter().position(|e| (e.1 - e.0) >= (file.1 - file.0)) {
            let spot = &mut empty[index];
            if spot.1 > file.0 {
                continue;
            }

            let elem = vec[file.0];
            for (fi, i) in (file.0..=file.1).zip(spot.0..=spot.1) {
                vec[i] = elem;
                vec[fi] = -1;
            }
            empty = find_empty_sections(&vec);
        }
    }
    //print(&vec);
    //println!("{:?}", secs);
    //println!("{:?}", empty);

    checksum(&vec)
    /*vec.into_iter()
    .enumerate()
    .filter(|(_i, n)| **n != -1)
    .map(|(i, n)| i * *n as usize)
    .sum()*/
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
