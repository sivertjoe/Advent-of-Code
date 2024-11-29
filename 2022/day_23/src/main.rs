pub type HashSet<V> = std::collections::HashSet<V, BuildHasherDefault<FxHasher>>;
pub type HashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

const MOV: [[(isize, isize); 3]; 4] = [
    [(0, -1), (1, -1), (-1, -1)],
    [(0, 1), (1, 1), (-1, 1)],
    [(-1, 0), (-1, -1), (-1, 1)],
    [(1, 0), (1, -1), (1, 1)],
];

type Point = (isize, isize);

#[inline]
fn has_neighbours(p: &Point, positions: &[[bool; DIM]; DIM]) -> bool
{
    (-1..=1).any(|x| {
        (-1..=1).any(|y| !(x == 0 && y == 0) && positions[(p.1 + y) as usize][(p.0 + x) as usize])
    })
}

#[inline]
fn is_blocked(p: &Point, positions: &[[bool; DIM]; DIM], dir: usize) -> bool
{
    MOV[dir]
        .iter()
        .any(|(dx, dy)| positions[(p.1 + dy) as usize][(p.0 + dx) as usize])
}

#[inline]
fn step(p: &Point, dir: usize) -> Point
{
    let (dx, dy) = MOV[dir][0];
    (p.0 + dx, p.1 + dy)
}

fn next_moves(
    positions: &[[bool; DIM]; DIM],
    proposals: &mut HashMap<Point, Point>,
    direction: usize,
)
{
    let mut dupes = HashSet::default();
    for y in 0..DIM
    {
        for x in 0..DIM
        {
            if !positions[y][x]
            {
                continue;
            }
            let pos = (x as isize, y as isize);
            if has_neighbours(&pos, positions)
            {
                for dir in (0..4).map(|d| (direction + d) % 4)
                {
                    if !is_blocked(&pos, positions, dir)
                    {
                        let step = step(&pos, dir);
                        if proposals.insert(step, pos).is_some()
                        {
                            dupes.insert(step);
                        }
                        break;
                    }
                }
            }
        }
    }

    for dup in dupes
    {
        proposals.remove(&dup);
    }
}

#[inline]
fn update_positions(
    positions: &mut [[bool; DIM]; DIM],
    proposals: &mut HashMap<Point, Point>,
) -> bool
{
    let mut changed = false;
    for ((nx, ny), (ox, oy)) in proposals.drain()
    {
        positions[oy as usize][ox as usize] = false;
        positions[ny as usize][nx as usize] = true;
        changed = true;
    }
    changed
}

fn solve(positions: &mut [[bool; DIM]; DIM], num_iters: Option<usize>) -> usize
{
    let mut proposals = HashMap::default();
    let mut num = 1;
    let mut dir = 0;

    next_moves(positions, &mut proposals, dir);
    while update_positions(positions, &mut proposals)
    {
        if num == num_iters.unwrap_or(usize::MAX)
        {
            break;
        }
        dir = (dir + 1) % 4;
        next_moves(positions, &mut proposals, dir);
        num += 1;
    }
    num
}

fn bounding_box(positions: &[[bool; DIM]; DIM]) -> (Point, Point)
{
    let mut min_x = usize::MAX;
    let mut min_y = usize::MAX;
    let mut max_x = usize::MIN;
    let mut max_y = usize::MIN;

    for y in 0..DIM
    {
        for x in 0..DIM
        {
            if positions[y][x] && y < min_y
            {
                min_y = y;
            }
            if positions[y][x] && x < min_x
            {
                min_x = x;
            }
            if positions[y][x] && y > max_y
            {
                max_y = y;
            }
            if positions[y][x] && x > max_x
            {
                max_x = x;
            }
        }
    }

    ((min_x as isize, min_y as isize), (max_x as isize, max_y as isize))
}

fn count(positions: &[[bool; DIM]; DIM]) -> usize
{
    let (min, max) = bounding_box(positions);

    let mut res = 0;
    for y in min.1..=max.1
    {
        for x in min.0..=max.0
        {
            if !positions[y as usize][x as usize]
            {
                res += 1;
            }
        }
    }
    res
}

// I've tuned it to require (almost) as little space as possible to reduce
// iterations. Not the cleanest overall solution.. but pretty fast!
const DIM: usize = 140;
const OFFSET: usize = 15;

fn parse(input: &[String]) -> [[bool; DIM]; DIM]
{
    let mut array: [[bool; DIM]; DIM] = [[false; DIM]; DIM];

    for (y, line) in input.iter().enumerate()
    {
        for (x, b) in line.bytes().enumerate()
        {
            if b == b'#'
            {
                array[y + OFFSET][x + OFFSET] = true;
            }
        }
    }
    array
}

fn task_one(input: &[String]) -> usize
{
    let mut elves = parse(input);
    solve(&mut elves, Some(10));
    count(&elves)
}

fn task_two(input: &[String]) -> usize
{
    let mut elves = parse(input);
    solve(&mut elves, None)
}

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn read_input<P>(path: P) -> Vec<String>
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path).unwrap().lines().map(String::from).collect()
}

enum Task
{
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
    let elapsed = t.elapsed().as_millis();

    match task
    {
        Task::One =>
        {
            println!("({}ms)\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        },
        Task::Two =>
        {
            println!("({}ms)\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        },
    };
}

fn get_input_file() -> String
{
    std::env::args().nth(1).unwrap_or_else(|| "input".to_string())
}
use core::{
    convert::TryInto,
    default::Default,
    hash::{BuildHasherDefault, Hasher},
    mem::size_of,
    ops::BitXor,
};

pub struct FxHasher
{
    hash: usize,
}

#[cfg(target_pointer_width = "32")]
const K: usize = 0x9e3779b9;
#[cfg(target_pointer_width = "64")]
const K: usize = 0x517cc1b727220a95;

impl Default for FxHasher
{
    #[inline]
    fn default() -> FxHasher
    {
        FxHasher {
            hash: 0
        }
    }
}

impl FxHasher
{
    #[inline]
    fn add_to_hash(&mut self, i: usize)
    {
        self.hash = self.hash.rotate_left(5).bitxor(i).wrapping_mul(K);
    }
}

impl Hasher for FxHasher
{
    #[inline]
    fn write(&mut self, mut bytes: &[u8])
    {
        #[cfg(target_pointer_width = "32")]
        let read_usize = |bytes: &[u8]| u32::from_ne_bytes(bytes[..4].try_into().unwrap());
        #[cfg(target_pointer_width = "64")]
        let read_usize = |bytes: &[u8]| u64::from_ne_bytes(bytes[..8].try_into().unwrap());

        let mut hash = FxHasher {
            hash: self.hash
        };
        assert!(size_of::<usize>() <= 8);
        while bytes.len() >= size_of::<usize>()
        {
            hash.add_to_hash(read_usize(bytes) as usize);
            bytes = &bytes[size_of::<usize>()..];
        }
        if (size_of::<usize>() > 4) && (bytes.len() >= 4)
        {
            hash.add_to_hash(u32::from_ne_bytes(bytes[..4].try_into().unwrap()) as usize);
            bytes = &bytes[4..];
        }
        if (size_of::<usize>() > 2) && bytes.len() >= 2
        {
            hash.add_to_hash(u16::from_ne_bytes(bytes[..2].try_into().unwrap()) as usize);
            bytes = &bytes[2..];
        }
        if (size_of::<usize>() > 1) && bytes.len() >= 1
        {
            hash.add_to_hash(bytes[0] as usize);
        }
        self.hash = hash.hash;
    }

    #[inline]
    fn write_u8(&mut self, i: u8)
    {
        self.add_to_hash(i as usize);
    }

    #[inline]
    fn write_u16(&mut self, i: u16)
    {
        self.add_to_hash(i as usize);
    }

    #[inline]
    fn write_u32(&mut self, i: u32)
    {
        self.add_to_hash(i as usize);
    }

    #[cfg(target_pointer_width = "32")]
    #[inline]
    fn write_u64(&mut self, i: u64)
    {
        self.add_to_hash(i as usize);
        self.add_to_hash((i >> 32) as usize);
    }

    #[cfg(target_pointer_width = "64")]
    #[inline]
    fn write_u64(&mut self, i: u64)
    {
        self.add_to_hash(i as usize);
    }

    #[inline]
    fn write_usize(&mut self, i: usize)
    {
        self.add_to_hash(i);
    }

    #[inline]
    fn finish(&self) -> u64
    {
        self.hash as u64
    }
}
