use std::collections::*;

#[derive(Clone, Copy)]
struct Direction {
    row: isize,
    col: isize,
}

impl Direction {
    const fn new(row: isize, col: isize) -> Self {
        Self { row, col }
    }
}

struct Edge {
    end: Point,
    dist: usize,
}

#[derive(Hash, Eq, PartialEq, Clone, Copy)]
struct Point {
    row: usize,
    col: usize,
}

impl Point {
    fn new((row, col): (usize, usize)) -> Self {
        Self { row, col }
    }
    fn add_dir(&self, dir: Direction) -> Option<Self> {
        let col = self.col.checked_add_signed(dir.col)?;
        let row = self.row.checked_add_signed(dir.row)?;
        Some(Point { col, row })
    }
}

const MOTIONS: [Direction; 4] = [
    Direction::new(-1, 0),
    Direction::new(0, 1),
    Direction::new(1, 0),
    Direction::new(0, -1),
];

fn in_limits(map: &[Vec<u8>], row: usize, col: usize) -> bool {
    row < map.len() && col < map[0].len()
}

fn can_go_there(current: &Point, next: &Point, map: &[Vec<u8>]) -> bool {
    let ch = map[next.row][next.col];
    if current.row > next.row {
        ch != b'v'
    } else if current.row < next.row {
        ch != b'^'
    } else if current.col > next.col {
        ch != b'>'
    } else {
        ch != b'<'
    }
}

fn find_all_points(map: &[Vec<u8>], start: Point, end: Point) -> HashSet<Point> {
    let mut stack: VecDeque<Point> = VecDeque::new();
    let mut seen: Vec<Vec<bool>> = vec![vec![false; map[0].len()]; map.len()];
    let mut nodes: HashSet<Point> = HashSet::new();
    nodes.insert(start);
    nodes.insert(end);

    stack.push_back(start);
    while let Some(current) = stack.pop_front() {
        seen[current.row][current.col] = true;
        let mut count = 0;
        for next in MOTIONS
            .iter()
            .filter_map(|dir| current.add_dir(*dir))
            .filter(|next| in_limits(map, next.row, next.col) && map[next.row][next.col] != b'#')
        {
            count += 1;
            if !seen[next.row][next.col] {
                stack.push_back(next);
            }
        }
        if count > 2 {
            nodes.insert(current);
        }
    }
    nodes
}

fn get_edges(map: &[Vec<u8>], nodes: &HashSet<Point>, part_two: bool) -> HashMap<Point, Vec<Edge>> {
    let mut edges: HashMap<Point, Vec<Edge>> = HashMap::new();

    for node in nodes.iter() {
        let mut seen: Vec<Vec<bool>> = vec![vec![false; map[0].len()]; map.len()];

        let mut stack: VecDeque<(Point, usize)> = VecDeque::new();
        stack.push_back((*node, 0));

        while let Some((current, current_dist)) = stack.pop_front() {
            seen[current.row][current.col] = true;

            for next in MOTIONS.iter().filter_map(|dir| current.add_dir(*dir)) {
                if in_limits(map, next.row, next.col)
                    && map[next.row][next.col] != b'#'
                    && !seen[next.row][next.col]
                    && (can_go_there(&current, &next, map) || part_two)
                {
                    let next_dist = current_dist + 1;

                    if !nodes.contains(&next) {
                        stack.push_back((next, next_dist));
                    } else {
                        let vec = edges.entry(*node).or_default();
                        let edge = Edge {
                            end: next,
                            dist: next_dist,
                        };
                        vec.push(edge);
                        let edge = Edge {
                            end: *node,
                            dist: next_dist,
                        };
                        vec.push(edge);
                    }
                }
            }
        }
    }
    edges
}

fn solve(input: &[String], part_two: bool) -> usize {
    let map: Vec<Vec<u8>> = input.iter().map(|s| s.as_bytes().to_vec()).collect();
    let start = Point::new((0, input[0].chars().position(|ch| ch == '.').unwrap()));
    let end = Point::new((
        input.len() - 1,
        input
            .last()
            .unwrap()
            .chars()
            .position(|ch| ch == '.')
            .unwrap(),
    ));

    let nodes = find_all_points(&map, start, end);
    let edges = get_edges(&map, &nodes, part_two);

    let mut seen: Vec<Vec<bool>> = vec![vec![false; map[0].len()]; map.len()];
    let mut stack: VecDeque<(Point, usize)> = VecDeque::new();
    stack.push_back((start, 0));
    let mut max_dist = 0;
    dfs(&mut stack, &edges, &mut max_dist, &end, &mut seen);
    max_dist
}

fn dfs(
    stack: &mut VecDeque<(Point, usize)>,
    edges: &HashMap<Point, Vec<Edge>>,
    max_dist: &mut usize,
    dest: &Point,
    seen: &mut Vec<Vec<bool>>,
) {
    let Some((current, dist)) = stack.back().cloned() else {
        return;
    };

    seen[current.row][current.col] = true;
    if current == *dest {
        *max_dist = std::cmp::max(*max_dist, dist);
        seen[current.row][current.col] = false;
    } else {
        for edge in edges.get(&current).unwrap() {
            let next = &edge.end;
            let next_dist = dist + edge.dist;
            if !seen[next.row][next.col] {
                stack.push_back((*next, next_dist));
                dfs(stack, edges, max_dist, dest, seen);
                stack.pop_back();
            }
        }
    }
    seen[current.row][current.col] = false;
}

fn task_one(input: &[String]) -> usize {
    solve(input, false)
}

fn task_two(input: &[String]) -> usize {
    solve(input, true)
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
