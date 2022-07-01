enum Fold
{
    Vertically(i32),
    Horizontally(i32),
}

struct Matrix
{
    vec: Vec<i32>,
    dim: (i32, i32),
}

impl std::ops::Index<[i32; 2]> for Matrix
{
    type Output = i32;

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

impl From<String> for Fold
{
    fn from(s: String) -> Self
    {
        let (letter, num) = s
            .split(' ')
            .nth(2)
            .unwrap()
            .split_once('=')
            .map(|(a, b)| (a.parse::<char>().unwrap(), b.parse::<i32>().unwrap()))
            .unwrap();

        match letter
        {
            'y' => Fold::Vertically(num),
            'x' => Fold::Horizontally(num),
            _ => unreachable!(),
        }
    }
}

fn read_input<P>(path: P) -> (Vec<Fold>, Vec<(i32, i32)>)
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();

    let mut coords = Vec::new();
    let mut folds = Vec::new();

    let mut b = true;
    for elem in std::io::BufReader::new(file).lines().flatten()
    {
        if elem.is_empty()
        {
            b = false;
            continue;
        }
        if b
        {
            coords.push(
                elem.split_once(',')
                    .map(|(a, b)| (a.parse::<i32>().unwrap(), b.parse::<i32>().unwrap()))
                    .unwrap(),
            );
        }
        else
        {
            folds.push(Fold::from(elem));
        }
    }
    (folds, coords)
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

fn main()
{
    let input = read_input("input");
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn task_one((folds, coords): &(Vec<Fold>, Vec<(i32, i32)>)) -> i32
{
    let matrix = matrix_create(coords);
    let matrix = matrix_fold(matrix, folds.first().unwrap());

    matrix.vec.iter().filter(|x| **x != 0).count() as i32
}

fn task_two((folds, coords): &(Vec<Fold>, Vec<(i32, i32)>)) -> Matrix
{
    folds.iter().fold(matrix_create(coords), matrix_fold)
}

fn matrix_create(coords: &[(i32, i32)]) -> Matrix
{
    let x = coords.iter().map(|(x, _)| x).max().unwrap() + 1;
    let y = coords.iter().map(|(_, y)| y).max().unwrap() + 1;
    let mut matrix = Matrix {
        vec: vec![0; (x * y) as usize], dim: (x, y)
    };

    for (x, y) in coords
    {
        matrix[[*x, *y]] = 1;
    }

    matrix
}

fn matrix_fold(matrix: Matrix, fold: &Fold) -> Matrix
{
    match *fold
    {
        Fold::Horizontally(n) => matrix_fold_horizontally(matrix, n),
        Fold::Vertically(n) => matrix_fold_vertically(matrix, n),
    }
}

fn matrix_fold_vertically(matrix: Matrix, n: i32) -> Matrix
{
    let dim = (matrix.dim.0, matrix.dim.1 / 2);
    let mut new_matrix = Matrix {
        dim,
        vec: vec![0; (dim.0 * dim.1) as usize],
    };

    for y in 0..n
    {
        for x in 0..matrix.dim.0
        {
            let _y = matrix.dim.1 - y - 1;
            new_matrix[[x, y]] = matrix[[x, y]] + matrix[[x, _y]];
        }
    }

    new_matrix
}

fn matrix_fold_horizontally(matrix: Matrix, n: i32) -> Matrix
{
    let dim = (matrix.dim.0 / 2, matrix.dim.1);
    let mut new_matrix = Matrix {
        dim,
        vec: vec![0; (dim.0 * dim.1) as usize],
    };
    for y in 0..new_matrix.dim.1
    {
        for x in 0..n
        {
            let _x = matrix.dim.0 - x - 1;
            new_matrix[[x, y]] = matrix[[x, y]] + matrix[[_x, y]];
        }
    }
    new_matrix
}

impl std::fmt::Display for Matrix
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        let mut builder = String::new();
        for i in 0..self.vec.len()
        {
            if i as i32 % self.dim.0 == 0
            {
                builder.push('\n');
            }
            if self.vec[i] == 0
            {
                builder.push('.');
            }
            else
            {
                builder.push('#');
            }
        }
        write!(f, "{}", builder)
    }
}
