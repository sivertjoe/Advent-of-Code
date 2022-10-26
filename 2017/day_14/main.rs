use std::collections::*;

fn round(list: &mut [usize], input: &[usize], current_pos: &mut usize, skip_size: &mut usize)
{
    #[allow(non_snake_case)]
    let N = list.len();

    for length in input
    {
        let mut end = (*current_pos + length - 1) % N;
        let mut start = *current_pos;
        for _ in 0..length / 2
        {
            let temp = list[end];
            list[end] = list[start];
            list[start] = temp;

            end = (end as i32 - 1).rem_euclid(N as i32) as usize;
            start = (start + 1) % N;
        }
        *current_pos = (*current_pos + length + *skip_size) % N;
        *skip_size += 1;
    }
}
fn knot_hash(input: &str) -> String
{
    let mut list: Vec<usize> = (0..256).collect();
    let mut current_pos = 0;
    let mut skip_size = 0;

    let input: Vec<usize> = input
        .bytes()
        .map(|b| b as usize)
        .chain(vec![17, 31, 73, 47, 23].into_iter())
        .collect();

    for _ in 0..64
    {
        round(&mut list, &input, &mut current_pos, &mut skip_size);
    }

    list.chunks(16)
        .map(|range| range.iter().cloned().reduce(|acc, item| acc ^ item).unwrap())
        .fold(String::new(), |acc, item| format!("{}{:02x}", acc, item))
}

fn calc_row(input: &str, num: i32) -> Vec<i32>
{
    let input = format!("{}-{}", input, num);
    let hash = knot_hash(&input);

    let conv = |ch: char| format!("{:04b}", ch.to_digit(16).unwrap());

    let mut vec = Vec::with_capacity(128);
    for ch in hash.chars()
    {
        for letter in conv(ch).chars()
        {
            vec.push(if letter == '1' { 1 } else { 0 });
        }
    }

    vec
}

fn create_matrix(input: &str) -> Matrix<i32>
{
    let calc_row = |row: i32| calc_row(input, row);
    let vec = (0..128).into_iter().map(|row| calc_row(row)).flatten().collect::<Vec<i32>>();
    let dim = (128, 128);
    Matrix {
        dim,
        vec,
    }
}

fn get_group(x: usize, y: usize, mat: &Matrix<i32>, group: &mut HashSet<(usize, usize)>)
{
    group.insert((x, y));
    for p in [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        .into_iter()
        .filter(|p| (0..mat.dim.0).contains(&p.0) && (0..mat.dim.1).contains(&p.1))
    {
        if mat[[x, y]] == 1 && !group.contains(&p)
        {
            get_group(p.0, p.1, mat, group);
        }
    }
}

fn task_one(input: &[String]) -> i32
{
    let mat = create_matrix(&input[0]);
    mat.vec.into_iter().sum()
}

fn task_two(input: &[String]) -> i32
{
    let mat = create_matrix(&input[0]);
    let mut global: HashSet<(usize, usize)> = HashSet::new();

    let mut num_groups = 0;
    for y in 0..mat.dim.1
    {
        for x in 0..mat.dim.0
        {
            if mat[[x, y]] == 1 && global.insert((x, y))
            {
                let mut group = HashSet::new();
                get_group(x, y, &mat, &mut group);
                num_groups += 1;
                global.extend(group.into_iter());
            }
        }
    }
    num_groups
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

struct Matrix<T>
{
    vec: Vec<T>,
    dim: (usize, usize),
}

impl<T> Matrix<T>
where
    T: std::fmt::Display,
{
    #[allow(dead_code)]
    fn print(&self)
    {
        for i in 0..self.vec.len()
        {
            if i % self.dim.0 == 0
            {
                println!("");
            }
            print!("{}", self.vec[i]);
        }
        println!("");
    }
}

impl<T> std::ops::Index<[usize; 2]> for Matrix<T>
{
    type Output = T;

    fn index(&self, idx: [usize; 2]) -> &Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &self.vec[idx]
    }
}

impl<T> std::ops::IndexMut<[usize; 2]> for Matrix<T>
{
    fn index_mut(&mut self, idx: [usize; 2]) -> &mut Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &mut self.vec[idx]
    }
}
