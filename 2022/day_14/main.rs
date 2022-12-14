use std::collections::*;

// I use FxHasher just because it's much faster than the std hashing algoritm
// pub type Set<V> = HashSet<V, BuildHasherDefault<FxHasher>>;
pub type Set<V> = HashSet<V, BuildHasherDefault<FxHasher>>;

fn parse(input: &[String]) -> Set<(isize, isize)>
{
    let parse = |s: &str| {
        let (fst, snd) = s.split_once(',').unwrap();
        let fst = fst.parse::<isize>().unwrap();
        let snd = snd.parse::<isize>().unwrap();
        (fst, snd)
    };

    let mut map = Set::default();
    for line in input
    {
        let mut iter = line.split(" -> ");
        let mut start = parse(iter.next().unwrap());

        for next in iter
        {
            let next = parse(next);
            draw(start, next, &mut map);
            start = next;
        }
    }
    map
}

fn draw(p1: (isize, isize), p2: (isize, isize), map: &mut Set<(isize, isize)>)
{
    let min_y = p1.1.min(p2.1);
    let min_x = p1.0.min(p2.0);
    let max_y = p1.1.max(p2.1);
    let max_x = p1.0.max(p2.0);
    for y in min_y..=max_y
    {
        for x in min_x..=max_x
        {
            map.insert((x, y));
        }
    }
}

fn solve(input: &[String], flag: bool) -> usize
{
    let mut map = parse(input);
    let max_y = map.iter().fold(0, |acc, (_x, y)| std::cmp::max(acc, *y));

    let mut path = Vec::new();

    (0..)
        .position(|_| {
            let mut pos = path.pop().unwrap_or((500, 0));

            while pos.1 + 1 != max_y + 2
            {
                match [(0, 1), (-1, 1), (1, 1)]
                    .into_iter()
                    .find(|(dx, dy)| !map.contains(&(pos.0 + dx, pos.1 + dy)))
                {
                    Some((dx, dy)) =>
                    {
                        path.push(pos);
                        pos = (pos.0 + dx, pos.1 + dy);
                    },
                    _ => break,
                }
            }

            !if flag && pos.1 == max_y + 1 { false } else { map.insert(pos) }
        })
        .unwrap()
}

fn task_one(input: &[String]) -> usize
{
    solve(input, true)
}

fn task_two(input: &[String]) -> usize
{
    solve(input, false)
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
