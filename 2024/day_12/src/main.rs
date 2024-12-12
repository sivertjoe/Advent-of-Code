use std::collections::*;

fn neighbors(map: &[Vec<u8>], (y, x): (usize, usize)) -> impl Iterator<Item = (usize, usize)> {
    let my = map.len() as isize;
    let mx = map[0].len() as isize;
    let x = x as isize;
    let y = y as isize;
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        .into_iter()
        .filter(move |(y, x)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
        .map(|(y, x)| (y as usize, x as usize))
}

fn find_area_and_perimiter(
    (y, x): (usize, usize),
    map: &[Vec<u8>],
) -> (usize, usize, HashSet<(usize, usize)>) {
    let start = (y, x);
    let ch = map[y][x];

    let mut vec = vec![start];
    let mut seen = HashSet::new();

    let mut perim = 0;

    while let Some((ny, nx)) = vec.pop() {
        if map[ny][nx] != ch {
            continue;
        }

        if !seen.insert((ny, nx)) {
            continue;
        }

        let ns = neighbors(map, (ny, nx))
            .filter(|(_ny, _nx)| map[*_ny][*_nx] == ch)
            .collect::<Vec<_>>();

        perim += 4 - ns.len();

        for n in ns {
            vec.push(n);
        }
    }

    (seen.len(), perim, seen)
}

fn task_one(input: &[String]) -> usize {
    let map: Vec<Vec<_>> = input.iter().map(|line| line.bytes().collect()).collect();

    let mut sum = 0;
    let mut seen = HashSet::new();
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if seen.insert((y, x)) {
                let (a, p, s) = find_area_and_perimiter((y, x), &map);
                sum += a * p;

                for p in s {
                    seen.insert(p);
                }
            }
        }
    }

    sum
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Dir {
    Left,
    Right,
    Up,
    Down,
}

impl Dir {
    fn rotate_wall(&self) -> Self {
        match self {
            Dir::Up => Dir::Left,
            Dir::Left => Dir::Down,
            Dir::Down => Dir::Right,
            Dir::Right => Dir::Up,
        }
    }
    fn rotate_air(&self) -> Self {
        match self {
            Dir::Up => Dir::Right,
            Dir::Right => Dir::Down,
            Dir::Down => Dir::Left,
            Dir::Left => Dir::Up,
        }
    }
}
fn print_shape2(seen: &HashSet<P>) {
    if seen.is_empty() {
        return;
    }
    let max_y = seen.iter().map(|p| p.0).max().unwrap();
    let max_x = seen.iter().map(|p| p.1).max().unwrap();

    let min_y = seen.iter().map(|p| p.0).min().unwrap();
    let min_x = seen.iter().map(|p| p.1).min().unwrap();

    for y in min_y - 1..=max_y + 1 {
        for x in min_x - 1..=max_x + 1 {
            if seen.contains(&(y, x)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
    println!();
}

fn print_shape(seen: &HashSet<P>, pos: P, dir: Dir) {
    let max_y = seen.iter().map(|p| p.0).max().unwrap();
    let max_x = seen.iter().map(|p| p.1).max().unwrap();

    let min_y = seen.iter().map(|p| p.0).min().unwrap();
    let min_x = seen.iter().map(|p| p.1).min().unwrap();

    let hug = hug_pos(pos, dir);

    let ch = match dir {
        Dir::Up => '^',
        Dir::Down => 'v',
        Dir::Right => '>',
        Dir::Left => '<',
    };

    for y in min_y - 1..=max_y + 1 {
        for x in min_x - 1..=max_x + 1 {
            if (y, x) == hug {
                print!("O");
            } else if seen.contains(&(y, x)) {
                print!("#");
            } else if (y, x) == pos {
                print!("{ch}");
            } else {
                print!(".");
            }
        }
        println!();
    }
    println!();
}
type P = (isize, isize);

fn hug_pos(pos: P, dir: Dir) -> P {
    match dir {
        Dir::Up => (pos.0, pos.1 + 1),
        Dir::Down => (pos.0, pos.1 - 1),
        Dir::Right => (pos.0 + 1, pos.1),
        Dir::Left => (pos.0 - 1, pos.1),
    }
}

fn try_move(pos: P, dir: Dir, seen: &HashSet<P>) -> Result<Result<P, (P, Dir)>, Dir> {
    let next = match dir {
        Dir::Left => (pos.0, pos.1 - 1),
        Dir::Right => (pos.0, pos.1 + 1),
        Dir::Up => (pos.0 - 1, pos.1),
        Dir::Down => (pos.0 + 1, pos.1),
    };

    if seen.contains(&next) {
        return Err(dir.rotate_wall());
    }

    let hug = hug_pos(next, dir);
    if seen.contains(&hug) {
        return Ok(Ok(next));
    }

    let dir = dir.rotate_air();
    let next = match dir {
        Dir::Left => (next.0, next.1 - 1),
        Dir::Right => (next.0, next.1 + 1),
        Dir::Up => (next.0 - 1, next.1),
        Dir::Down => (next.0 + 1, next.1),
    };
    Ok(Err((next, dir)))
}

fn calculate_sides(seen: &HashSet<(usize, usize)>) -> usize {
    if seen.is_empty() {
        return 0;
    }
    let seen = seen
        .iter()
        .map(|(y, x)| (*y as isize, *x as isize))
        .collect::<HashSet<_>>();

    //println!("{:?}", seen);

    let start = seen.iter().min().unwrap();
    let start = (start.0 - 1, start.1);
    let mut dir = Dir::Down;

    /*let start = seen
        .iter()
        .map(|&(y, x)| [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)])
        .flatten()
        .find(|pos| !seen.contains(pos))
        .unwrap();

    let mut dir = if seen.contains(&(start.0 - 1, start.1)) {
        Dir::Up
    } else if seen.contains(&(start.0 + 1, start.1)) {
        Dir::Down
    } else if seen.contains(&(start.0, start.1 - 1)) {
        Dir::Left
    } else {
        Dir::Right
    };*/

    dir = try_move(start, dir, &seen).unwrap_err();
    let odir = dir;
    let mut pos = start;

    let mut sum = 0;
    // print_shape(&seen, pos, dir);

    loop {
        match try_move(pos, dir, &seen) {
            Ok(Ok(next)) => pos = next,
            Ok(Err((next, ndir))) => {
                sum += 1;
                pos = next;
                dir = ndir;
            }
            Err(ndir) => {
                sum += 1;
                dir = ndir;
            }
        };
        if pos == start && dir == odir {
            break;
        }
        //print_shape(&seen, pos, dir);
        //pause();
    }

    sum
}

fn pause() {
    let mut _buf = String::new();
    let _ = std::io::stdin().read_line(&mut _buf);
}

type U = (usize, usize);

fn flood((y, x): U, map: &[Vec<u8>], ignore: u8) -> HashSet<(usize, usize)> {
    let start = (y, x);

    let mut vec = vec![start];
    let mut seen = HashSet::new();

    while let Some((ny, nx)) = vec.pop() {
        if map[ny][nx] == ignore {
            continue;
        }

        if !seen.insert((ny, nx)) {
            continue;
        }

        let ns = neighbors(map, (ny, nx))
            .filter(|(_ny, _nx)| map[*_ny][*_nx] != ignore)
            .collect::<Vec<_>>();

        for n in ns {
            vec.push(n);
        }
    }

    seen
}

fn find_insides(shape: &HashSet<U>, map: &[Vec<u8>], ingore: u8) -> Vec<HashSet<U>> {
    let max_y = shape.iter().map(|p| p.0).max().unwrap();
    let max_x = shape.iter().map(|p| p.1).max().unwrap();

    let min_y = shape.iter().map(|p| p.0).min().unwrap();
    let min_x = shape.iter().map(|p| p.1).min().unwrap();
    let mut inside = Vec::new();

    let mut seen = shape.clone();
    for y in min_y..=max_y {
        for x in min_x..=max_x {
            if !seen.contains(&(y, x)) {
                let s = flood((y as usize, x as usize), map, ingore);

                let goes_to_edge = s
                    .iter()
                    .any(|&(y, x)| y == 0 || x == 0 || y == map.len() - 1 || x == map[0].len() - 1);
                if !goes_to_edge {
                    for p in s.iter() {
                        seen.insert((p.0 as _, p.1 as _));
                    }
                    inside.push(s);
                }
            }
        }
    }

    inside
}

// Too high     4561146
//               811148
//               811684
// Too low :(    760812
// 795562

fn task_two(input: &[String]) -> usize {
    let map: Vec<Vec<_>> = input.iter().map(|line| line.bytes().collect()).collect();

    let mut sum = 0;
    let mut seen = HashSet::new();
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if map[y][x] == b'.' {
                continue;
            }
            if seen.insert((y, x)) {
                // let y = 0;
                // let x = 6;
                let (a, _p, s) = find_area_and_perimiter((y, x), &map);
                let mut sides = calculate_sides(&s);

                if map[y][x] == b'I' {
                    let s = s.iter().map(|p| (p.0 as _, p.1 as _)).collect();
                    print_shape2(&s);
                }

                let inside = find_insides(&s, &map, map[y][x]);
                for s in inside {
                    sides += calculate_sides(&s);
                    // let s = s.into_iter().map(|p| (p.0 as _, p.1 as _)).collect();
                    // print_shape2(&s);
                    if map[y][x] == b'I' {
                        let s = s.iter().map(|p| (p.0 as _, p.1 as _)).collect();
                        print_shape2(&s);
                    }
                }

                for p in s {
                    seen.insert(p);
                }
                println!("{} {} * {}", map[y][x] as char, a, sides);
                sum += a * sides;
                // std::process::exit(0);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_1() {
        let input = [
            "AAAA".to_string(),
            "BBCD".to_string(),
            "BBCC".to_string(),
            "EEEC".to_string(),
        ];

        assert_eq!(task_two(&input), 80);
    }
    #[test]
    fn input_2() {
        let input = [
            "OOOOO".to_string(),
            "OXOXO".to_string(),
            "OOOOO".to_string(),
            "OXOXO".to_string(),
            "OOOOO".to_string(),
        ];

        assert_eq!(task_two(&input), 436);
    }
    #[test]
    fn input_3() {
        let input = [
            "RRRRIICCFF".to_string(),
            "RRRRIICCCF".to_string(),
            "VVRRRCCFFF".to_string(),
            "VVRCCCJFFF".to_string(),
            "VVVVCJJCFE".to_string(),
            "VVIVCCJJEE".to_string(),
            "VVIIICJJEE".to_string(),
            "MIIIIIJJEE".to_string(),
            "MIIISIJEEE".to_string(),
            "MMMISSJEEE".to_string(),
        ];

        assert_eq!(task_two(&input), 1206);
    }
    #[test]
    fn input_4() {
        let input = [
            "EEEEE".to_string(),
            "EXXXX".to_string(),
            "EEEEE".to_string(),
            "EXXXX".to_string(),
            "EEEEE".to_string(),
        ];

        assert_eq!(task_two(&input), 236);
    }
    #[test]
    fn input_5() {
        let input = [
            "AAAAAA".to_string(),
            "AAABBA".to_string(),
            "AAABBA".to_string(),
            "ABBAAA".to_string(),
            "ABBAAA".to_string(),
            "AAAAAA".to_string(),
        ];

        assert_eq!(task_two(&input), 368);
    }
    #[test]
    fn input_6() {
        let input = [
            "AAAAAA".to_string(),
            "AAABBA".to_string(),
            "AAABBA".to_string(),
            "ABBAAA".to_string(),
            "ABBAAA".to_string(),
            "AAAAAA".to_string(),
        ];

        assert_eq!(task_two(&input), 368);
    }
    #[test]
    fn input_7() {
        let input = [
            "OOOOO".to_string(),
            "OXOXO".to_string(),
            "OXXXO".to_string(),
        ];

        assert_eq!(task_two(&input), 160);
    }
    #[test]
    fn input_8() {
        let input = [
            "CCAAA".to_string(),
            "CCAAA".to_string(),
            "AABBA".to_string(),
            "AAAAA".to_string(),
        ];

        assert_eq!(task_two(&input), 164);
    }

    #[test]
    fn input_9() {
        let input = [
            "AAAAAAAA".to_string(),
            "AACBBDDA".to_string(),
            "AACBBAAA".to_string(),
            "ABBAAAAA".to_string(),
            "ABBADDDA".to_string(),
            "AAAADADA".to_string(),
            "AAAAAAAA".to_string(),
        ];

        assert_eq!(task_two(&input), 946);
    }
    #[test]
    fn input_10() {
        let input = [
            "...................".to_string(),
            ".....#...#.#####...".to_string(),
            "....##...#######.#.".to_string(),
            "....##############.".to_string(),
            ".################..".to_string(),
            "..###############..".to_string(),
            ".....#########.....".to_string(),
            "......######.......".to_string(),
            ".......#####.......".to_string(),
            ".......######......".to_string(),
            "........#####......".to_string(),
            "......#####........".to_string(),
            "...................".to_string(),
        ];

        assert_eq!(task_two(&input), 96 * 496 * 42);
    }
}
