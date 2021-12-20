type MatItem = u8;

#[derive(Clone)]
struct Matrix
{
    vec: Vec<MatItem>,
    dim: (i32, i32),
}

impl Matrix
{
    #[allow(dead_code)]
    fn print(&self)
    {
        for i in 0..self.vec.len()
        {
            if i as i32 % self.dim.0 == 0
            {
                println!();
            }
            if self.vec[i] == 1
            {
                print!("#");
            }
            else
            {
                print!(".");
            }
        }
        println!();
    }
}


impl std::ops::Index<[i32; 2]> for Matrix
{
    type Output = MatItem;

    fn index(&self, idx: [i32; 2]) -> &Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &self.vec[idx]
    }
}

impl std::ops::IndexMut<[i32; 2]> for Matrix
{
    fn index_mut(&mut self, idx: [i32; 2]) -> &mut Self::Output
    {
        let idx = ((idx[1] * self.dim.0) as usize) + idx[0] as usize;
        &mut self.vec[idx]
    }
}
fn read_input<P>(path: P) -> (Vec<u8>, Matrix)
where
    P: AsRef<std::path::Path>,
{
    let mapper = |c: char| (c == '#') as u8;
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();

    let mut iter = std::io::BufReader::new(file).lines().flatten();

    let algo: Vec<u8> = iter.next().unwrap().chars().map(mapper).collect();

    let vec: Vec<Vec<_>> = iter.skip(1).map(|line| line.chars().map(mapper).collect()).collect();

    let dim = (vec[0].len() as i32, vec.len() as i32);
    let vec = vec.into_iter().flatten().collect();


    (algo, Matrix {
        dim,
        vec,
    })
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

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}


fn solve<const N: u8>(alg: &[u8], mat: &Matrix) -> i32
{
    (0..N)
        .fold(mat.clone(), |acc, i| step(acc, alg, i))
        .vec
        .into_iter()
        .map(|i| i as i32)
        .sum::<i32>()
}

fn task_one((algorithm, matrix): &(Vec<u8>, Matrix)) -> i32
{
    solve::<2>(algorithm, matrix)
}

fn task_two((algorithm, matrix): &(Vec<u8>, Matrix)) -> i32
{
    solve::<50>(algorithm, matrix)
}

fn get_number(x: i32, y: i32, mat: &Matrix, def: u8) -> usize
{
    let mut s = String::with_capacity(9);
    for j in -1..=1
    {
        for i in -1..=1
        {
            let _x = x + i;
            let _y = y + j;

            if _x < 0 || _x >= mat.dim.0 || _y < 0 || _y >= mat.dim.1
            {
                let c = std::char::from_digit(def as u32, 10).unwrap();
                s.push(c);
            }
            else
            {
                let c = std::char::from_digit(mat[[_x, _y]] as u32, 10).unwrap();
                s.push(c);
            }
        }
    }
    usize::from_str_radix(&s, 2).unwrap()
}

fn step(mat: Matrix, algo: &[u8], i: u8) -> Matrix
{
    // 🙃
    let default = if i % 2 != 0 { algo[0] } else { 0 };

    let dim = (mat.dim.0 + 2, mat.dim.1 + 2);
    let vec = vec![0; (dim.0 * dim.1) as usize];

    let mut new = Matrix {
        dim,
        vec,
    };

    for y in 0..dim.1
    {
        for x in 0..dim.0
        {
            let index = get_number(x - 1, y - 1, &mat, default);
            new[[x, y]] = algo[index];
        }
    }

    new
}
