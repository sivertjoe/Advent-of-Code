use std::collections::*;

fn parse(input: &[String]) -> Vec<Vec<usize>> {
    input
        .iter()
        .map(|line| {
            line.chars()
                .map(|b| b.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

fn neighbors(
    vec: &[Vec<usize>],
    idx: (usize, usize),
    (gdir, count): (Dir, usize),
) -> impl Iterator<Item = ((usize, usize), (Dir, usize))> {
    let my = vec.len() as isize;
    let mx = vec[0].len() as isize;
    let y = idx.0 as isize;
    let x = idx.1 as isize;
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        .into_iter()
        .filter(move |(y, x)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
        .map(|(y, x)| (y as usize, x as usize))
        .map(move |(y, x)| {
            let dir = if y == idx.0 {
                if x > idx.1 {
                    Dir::Right
                } else {
                    Dir::Left
                }
            } else if x == idx.1 {
                if y > idx.0 {
                    Dir::Down
                } else {
                    Dir::Up
                }
            } else {
                unreachable!();
            };

            (y, x, dir)
        })
        .filter(move |(y, x, dir)| {
            !((*dir == Dir::Up && gdir == Dir::Down)
                || (*dir == Dir::Down && gdir == Dir::Up)
                || (*dir == Dir::Left && gdir == Dir::Right)
                || (*dir == Dir::Right && gdir == Dir::Left)
                || (*dir == gdir && count == 2))
        })
        .map(move |(y, x, dir)| {
            let ncount = if dir == gdir { count + 1 } else { 0 };
            ((y, x), (dir, ncount))
        })
}

#[derive(Hash, Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Copy)]
enum Dir {
    Right,
    Left,
    Up,
    Down,
}

fn _print(vec: &[Vec<usize>], me: (usize, usize), dir: Dir) {
    let mech = match dir {
        Dir::Right => '>',
        Dir::Left => '<',
        Dir::Up => '^',
        Dir::Down => 'v',
    };
    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            if (y, x) == me {
                print!("{}", mech);
            } else {
                print!("{}", vec[y][x]);
            }
        }
        println!();
    }
    println!();
}

fn _print2(
    vec: &[Vec<usize>],
    prev: &HashMap<((usize, usize), (Dir, usize)), ((usize, usize), (Dir, usize))>,
    end: ((usize, usize), (Dir, usize)),
) {
    let mut list = Vec::new();
    let mut next = end;
    while let Some(elm) = prev.get(&next) {
        assert!(elm.1 .1 < 3);
        list.push(elm.clone());
        next = elm.clone();
    }

    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            if let Some(e) = list.iter().find(|e| y == e.0 .0 && x == e.0 .1) {
                let ch = match e.1 .0 {
                    Dir::Right => '>',
                    Dir::Left => '<',
                    Dir::Up => '^',
                    Dir::Down => 'v',
                };
                //print!("|{}{}|", ch, e.1 .1);
                print!("{}", ch);
            } else {
                print!("{}", vec[y][x]);
            }
        }
        println!();
    }
    println!();
}

fn _pause() {
    use std::io::BufRead;
    let _ = std::io::stdin().lock().read_line(&mut String::new());
}

fn task_one(input: &[String]) -> usize {
    let vec = parse(input);

    let mut seen = HashSet::new();

    let start = (0, 0);

    seen.insert((start, (Dir::Right, 0)));
    //seen.insert((start, 0));

    let mut stack = BinaryHeap::new();
    use std::cmp::Reverse;

    stack.push((Reverse(0), start, (Dir::Right, 0)));
    stack.push((Reverse(0), start, (Dir::Down, 0)));

    let end = (vec.len() - 1, vec[0].len() - 1);

    let mut prev = HashMap::new();

    // 1012
    // 1008??
    while let Some(_xd @ (cost, p, info)) = stack.pop() {
        //println!("begin");
        if p == end {
            //_print2(&vec, &prev, (p, info));

            return cost.0;
        }
        //println!("{:?} {}", stack.len(), cost.0);
        // _print(&vec, p, info.0);
        // _pause();

        //println!("huh?");
        for (ns, ninfo) in neighbors(&vec, p, info) {
            let new_cost = cost.0 + vec[ns.0][ns.1];
            //if seen.insert((ns, new_cost)) {
            if seen.insert((ns, ninfo)) {
                prev.insert((ns, ninfo), (p, info));
                stack.push((Reverse(new_cost), ns, ninfo));
            }
        }
        //println!("nvm");
    }

    unreachable!()
}

fn neighbors2(
    vec: &[Vec<usize>],
    idx: (usize, usize),
    (gdir, count): (Dir, usize),
) -> impl Iterator<Item = ((usize, usize), (Dir, usize))> {
    let my = vec.len() as isize;
    let mx = vec[0].len() as isize;
    let y = idx.0 as isize;
    let x = idx.1 as isize;
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        .into_iter()
        .filter(move |(y, x)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
        .map(|(y, x)| (y as usize, x as usize))
        .map(move |(y, x)| {
            let dir = if y == idx.0 {
                if x > idx.1 {
                    Dir::Right
                } else {
                    Dir::Left
                }
            } else if x == idx.1 {
                if y > idx.0 {
                    Dir::Down
                } else {
                    Dir::Up
                }
            } else {
                unreachable!();
            };

            (y, x, dir)
        })
        .filter(move |(_y, _x, dir)| {
            !((*dir == Dir::Up && gdir == Dir::Down)
                || (*dir == Dir::Down && gdir == Dir::Up)
                || (*dir == Dir::Left && gdir == Dir::Right)
                || (*dir == Dir::Right && gdir == Dir::Left)
                || (*dir == gdir && count == 9))
        })
        .filter(move |(_y, _x, dir)| {
            if count < 3 && gdir != *dir {
                false
            } else {
                true
            }
        })
        .map(move |(y, x, dir)| {
            let ncount = if dir == gdir { count + 1 } else { 0 };
            ((y, x), (dir, ncount))
        })
}

fn task_two(input: &[String]) -> usize {
    let vec = parse(input);
    let mut seen = HashSet::new();
    let start = (0, 0);
    seen.insert((start, (Dir::Right, 0)));

    let mut stack = BinaryHeap::new();
    use std::cmp::Reverse;

    stack.push((Reverse(0), start, (Dir::Right, 0)));
    stack.push((Reverse(0), start, (Dir::Down, 0)));

    let end = (vec.len() - 1, vec[0].len() - 1);

    while let Some((cost, p, info)) = stack.pop() {
        if p == end && info.1 >= 3 {
            return cost.0;
        }

        for (ns, ninfo) in neighbors2(&vec, p, info) {
            let new_cost = cost.0 + vec[ns.0][ns.1];
            if seen.insert((ns, ninfo)) {
                stack.push((Reverse(new_cost), ns, ninfo));
            }
        }
    }

    unreachable!()
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
