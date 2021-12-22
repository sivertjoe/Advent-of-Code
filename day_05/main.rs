struct Line
{
    p1: Point,
    p2: Point,
}

struct Point
{
    x: i32,
    y: i32,
}

struct Map
{
    dim: i32,
    map: Vec<i32>,
}

impl Map
{
    // We have an NxN grid
    fn new(lines: &[Line]) -> Self
    {
        let dim = Self::get_dim(lines);
        let map = vec![0; (dim * dim) as usize];
        Self {
            dim,
            map,
        }
    }

    #[inline]
    fn get_dim(lines: &[Line]) -> i32
    {
        lines
            .iter()
            .map(|p| if p.p1.x > p.p2.x { p.p1.x } else { p.p2.x })
            .max()
            .unwrap()
            + 1
    }
}

impl std::ops::Index<[i32; 2]> for Map
{
    type Output = i32;

    fn index(&self, idx: [i32; 2]) -> &Self::Output
    {
        let index = ((idx[1] * self.dim) + idx[0]) as usize;
        &self.map[index]
    }
}

impl std::ops::IndexMut<[i32; 2]> for Map
{
    fn index_mut(&mut self, idx: [i32; 2]) -> &mut Self::Output
    {
        let index = ((idx[1] * self.dim) + idx[0]) as usize;
        &mut self.map[index]
    }
}

impl std::str::FromStr for Line
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let (p1, p2): (Point, Point) = s
            .split_once("->")
            .map(|(x, y)| (x.trim().parse().unwrap(), y.trim().parse().unwrap()))
            .unwrap();
        Ok(Self {
            p1,
            p2,
        })
    }
}

impl std::str::FromStr for Point
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let (x, y): (i32, i32) = s
            .split_once(',')
            .map(|(x, y)| (x.parse().unwrap(), y.parse().unwrap()))
            .unwrap();
        Ok(Self {
            x,
            y,
        })
    }
}

fn read_input<T, P>(path: P) -> Vec<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    std::io::BufReader::new(file)
        .lines()
        .flatten()
        .map(|line| line.parse::<T>().unwrap())
        .collect()
}

fn time<F, T, U>(pre: &'static str, f: F, arg: T)
where
    F: Fn(T) -> U,
    U: std::fmt::Display,
{
    let t0 = std::time::Instant::now();
    let res = f(arg);
    let t1 = std::time::Instant::now();
    println!("Task {}: {}\t({}ms)", pre, res, t1.duration_since(t0).as_millis());
}

fn main()
{
    let vec = read_input("input");
    let mut map = Map::new(&vec);
    time("one", task_one, (&vec, &mut map));
    time("two", task_two, (&vec, &mut map));
}

fn func<F, C>(lines: &[Line], map: &mut Map, condition: C, f: F) -> i32
where
    C: Fn(&Line) -> bool,
    F: Fn(&Line, &mut Map),
{
    for line in lines
    {
        if condition(line)
        {
            f(line, map);
        }
    }

    map.map.iter().filter(|&&c| c > 1).count() as i32
}

fn task_one((lines, map): (&[Line], &mut Map)) -> i32
{
    func(lines, map, is_horizontal_or_vertical, draw_line)
}

fn task_two((lines, map): (&[Line], &mut Map)) -> i32
{
    func(lines, map, is_diagonal, draw_diag)
}

#[inline]
fn is_horizontal_or_vertical(line: &Line) -> bool
{
    line.p1.x == line.p2.x || line.p1.y == line.p2.y
}

#[inline]
fn is_diagonal(line: &Line) -> bool
{
    !is_horizontal_or_vertical(line)
}

fn draw_line(line: &Line, map: &mut Map)
{
    if line.p1.x == line.p2.x
    {
        // Vertical
        let x = line.p1.x;
        let (start, end) =
            if line.p1.y < line.p2.y { (line.p1.y, line.p2.y) } else { (line.p2.y, line.p1.y) };
        for y in start..=end
        {
            map[[x, y]] += 1;
        }
    }
    else
    {
        // Horizontal
        let y = line.p1.y;
        let (start, end) =
            if line.p1.x < line.p2.x { (line.p1.x, line.p2.x) } else { (line.p2.x, line.p1.x) };
        for x in start..=end
        {
            map[[x, y]] += 1;
        }
    }
}

fn draw_diag(line: &Line, map: &mut Map)
{
    let (x, mut y, end, inc) = if line.p1.x > line.p2.x
    {
        let inc = if line.p1.y < line.p2.y { -1 } else { 1 };
        (line.p2.x, line.p2.y, line.p1.x, inc)
    }
    else
    {
        let inc = if line.p2.y < line.p1.y { -1 } else { 1 };
        (line.p1.x, line.p1.y, line.p2.x, inc)
    };

    for x in x..=end
    {
        map[[x, y]] += 1;
        y += inc;
    }
}
