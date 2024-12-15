use std::collections::*;

fn parse(input: &[String]) -> (HashMap<(usize, usize), char>, Vec<char>) {
    let mut iter = input.split(|line| line.is_empty());

    let mut map = HashMap::new();
    for (y, line) in iter.next().unwrap().iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            map.insert((y, x), ch);
        }
    }

    let mut vec = Vec::new();
    for line in iter.next().unwrap().iter() {
        let tmp = line.chars().collect::<Vec<_>>();
        vec.extend(tmp);
    }
    (map, vec)
}
type P = (usize, usize);
type I = (isize, isize);

fn print_map(map: &HashMap<P, char>, me: P) {
    let min_y = map.keys().map(|k| k.0).min().unwrap();
    let min_x = map.keys().map(|k| k.1).min().unwrap();

    let max_y = map.keys().map(|k| k.0).max().unwrap();
    let max_x = map.keys().map(|k| k.1).max().unwrap();

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            if (y, x) == me {
                print!("@");
            } else {
                let ch = map.get(&(y, x)).unwrap();
                print!("{}", ch);
            }
        }
        println!();
    }
    println!()
}

fn dir_to_inc(ch: char) -> (isize, isize) {
    match ch {
        '^' => (-1, 0),
        'v' => (1, 0),
        '<' => (0, -1),
        '>' => (0, 1),
        _ => unreachable!(),
    }
}

fn p(pos: P, inc: I) -> P {
    (
        pos.0.checked_add_signed(inc.0).unwrap(),
        pos.1.checked_add_signed(inc.1).unwrap(),
    )
}
fn pn(pos: P, inc: I, n: isize) -> P {
    (
        pos.0.checked_add_signed(n * inc.0).unwrap(),
        pos.1.checked_add_signed(n * inc.1).unwrap(),
    )
}

fn try_move(pos: P, inc: I, map: &HashMap<P, char>) -> Option<usize> {
    let mut boxpos = p(pos, inc);
    for i in 1.. {
        let next = p(boxpos, inc);
        match map.get(&next).unwrap() {
            '.' => return Some(i),
            '#' => return None,
            'O' | '[' | ']' => {}
            _ => unreachable!(),
        };
        boxpos = next;
    }
    unreachable!()
}

fn expand_map(map: HashMap<P, char>) -> HashMap<P, char> {
    let mut new = HashMap::new();

    let min_y = map.keys().map(|k| k.0).min().unwrap();
    let min_x = map.keys().map(|k| k.1).min().unwrap();

    let max_y = map.keys().map(|k| k.0).max().unwrap();
    let max_x = map.keys().map(|k| k.1).max().unwrap();

    let mut vec = Vec::new();
    for y in min_y..=max_y {
        let mut temp = Vec::new();
        for x in min_x..=max_x {
            let ch = map.get(&(y, x)).unwrap();
            match ch {
                '#' => {
                    temp.push('#');
                    temp.push('#');
                }
                '.' => {
                    temp.push('.');
                    temp.push('.');
                }
                'O' => {
                    temp.push('[');
                    temp.push(']');
                }
                '@' => {
                    temp.push('@');
                    temp.push('.');
                }
                _ => unreachable!(),
            }
        }
        vec.push(temp);
    }

    for (y, vec) in vec.into_iter().enumerate() {
        for (x, ch) in vec.into_iter().enumerate() {
            new.insert((y, x), ch);
        }
    }

    new
}

fn do_move(pos: P, inc: I, times: usize, map: &mut HashMap<P, char>) {
    for i in 1..=times {
        let curr_box = pn(pos, inc, i as isize);
        let next = p(curr_box, inc);

        assert!(matches!(map.get(&curr_box), Some('O')));
        if i == 1 {
            map.insert(curr_box, '.');
        }
        map.insert(next, 'O');
    }
}

fn task_one(input: &[String]) -> usize {
    let (mut map, vec) = parse(input);

    let mut pos = map
        .iter()
        .find_map(|k| (*k.1 == '@').then_some(*k.0))
        .unwrap();

    map.insert(pos, '.');

    for r#move in vec {
        let inc = dir_to_inc(r#move);
        let next = (
            pos.0.checked_add_signed(inc.0).unwrap(),
            pos.1.checked_add_signed(inc.1).unwrap(),
        );

        let elem = *map.get(&next).unwrap();
        match elem {
            '.' => {
                pos = next;
            }
            '#' => {}
            'O' => {
                if let Some(times) = try_move(pos, inc, &map) {
                    do_move(pos, inc, times, &mut map);
                    pos = next;
                }
            }
            _ => unreachable!(),
        }
    }

    map.into_iter()
        .filter(|k| k.1 == 'O')
        .map(|k| 100 * k.0 .0 + k.0 .1)
        .sum()
}

fn try_move2(pos: P, inc: I, map: &HashMap<P, char>) -> Option<Vec<P>> {
    let boxpos = p(pos, inc);
    if inc.1 != 0 {
        return try_move(pos, inc, map).map(|n| {
            (1..=n)
                .filter(|n| n % 2 == 0)
                .map(|n| (boxpos.0, pn(pos, inc, n as isize)).1)
                .collect::<Vec<_>>()
        });
    }

    let (left, right) = if *map.get(&boxpos).unwrap() == ']' {
        ((boxpos.0, boxpos.1 - 1), boxpos)
    } else {
        (boxpos, (boxpos.0, boxpos.1 + 1))
    };

    let mut vec = Vec::new();
    for i in 1.. {
        let next_left = p(left, inc);
        let next_right = p(right, inc);
        vec.push(left);

        match (map.get(&next_left).unwrap(), map.get(&next_right).unwrap()) {
            ('.', '.') => return Some(vec),
            ('.', '[') | ('.', ']') => {
                return try_move2(right, inc, map).map(|mut other| {
                    other.extend(vec);
                    other
                });
            }
            (']', '.') => {
                return try_move2(left, inc, map).map(|mut other| {
                    other.extend(vec);
                    other
                })
            }
            (']', '[') | ('[', ']') => {
                if let (Some(l), Some(r)) = (try_move2(left, inc, map), try_move2(right, inc, map))
                {
                    vec.extend(l);
                    vec.extend(r);
                    return Some(vec);
                } else {
                    return None;
                }
            }

            (l, r) if *l == '#' || *r == '#' => {
                return None;
            }
            e => {
                println!("{:?}", e);
                print_map(map, left);
                unreachable!()
            }
        }
    }

    todo!()
}

fn do_move2(mut boxes: Vec<P>, inc: I, map: &mut HashMap<P, char>) {
    // println!("BOXES {:?}", boxes);
    let mut to_clear = Vec::new();

    boxes.sort();
    for b in &boxes {
        let arr = if *map.get(&b).unwrap() == ']' {
            [(b.0, b.1 - 1), *b]
        } else {
            [*b, (b.0, b.1 + 1)]
        };
        for pos in arr {
            //map.insert(pos, '.');
            to_clear.push(pos);
        }
    }
    // print_map(map, (0, 0));

    let mut to_do = Vec::new();
    for b in boxes {
        let mut arr = if *map.get(&b).unwrap() == ']' {
            vec![(b.0, b.1 - 1), b]
        } else {
            vec![b, (b.0, b.1 + 1)]
        };
        arr.sort();
        // println!("b{:?} -> arr {:?}", b, arr);

        for (pos, ch) in [(arr[0], '['), (arr[1], ']')].into_iter() {
            let next = p(pos, inc);
            //map.insert(next, ch);
            to_do.push((next, ch));
        }
    }
    for pos in to_clear {
        map.insert(pos, '.');
    }
    for (pos, ch) in to_do {
        map.insert(pos, ch);
    }
}

fn task_two(input: &[String]) -> usize {
    let (map, vec) = parse(input);
    let mut map = expand_map(map);
    let mut pos = map
        .iter()
        .find_map(|k| (*k.1 == '@').then_some(*k.0))
        .unwrap();

    map.insert(pos, '.');
    // print_map(&map, pos);
    for r#move in vec {
        let inc = dir_to_inc(r#move);
        let next = (
            pos.0.checked_add_signed(inc.0).unwrap(),
            pos.1.checked_add_signed(inc.1).unwrap(),
        );

        let elem = *map.get(&next).unwrap();
        match elem {
            '.' => {
                pos = next;
            }
            '#' => {}
            '[' | ']' => {
                if let Some(boxes) = try_move2(pos, inc, &map) {
                    do_move2(boxes, inc, &mut map);
                    pos = next;
                    map.insert(pos, '.');
                }
            }
            _ => unreachable!(),
        }
        // print_map(&map, pos);
        // let _ = std::io::stdin().read_line(&mut String::new());
    }
    // print_map(&map, pos);

    map.into_iter()
        .filter(|k| k.1 == '[')
        .map(|k| 100 * k.0 .0 + k.0 .1)
        .sum()
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
