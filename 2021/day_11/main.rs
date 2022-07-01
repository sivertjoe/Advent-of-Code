#[derive(Clone)]
struct Octopus
{
    energy:     i32,
    has_glowed: bool,
}


#[derive(Clone)]
struct Matrix
{
    vec: Vec<Octopus>,
    dim: (i32, i32),
}

impl std::ops::Index<[i32; 2]> for Matrix
{
    type Output = Octopus;

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

impl std::str::FromStr for Matrix
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let vec: Vec<_> = s
            .lines()
            .map(|line| {
                line.as_bytes()
                    .iter()
                    .map(|b| Octopus {
                        energy: (b - b'0') as i32, has_glowed: false
                    })
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect();
        let dim = (10, 10);

        Ok(Self {
            vec,
            dim,
        })
    }
}

fn read_input<P>(path: P) -> Matrix
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path).unwrap().parse().unwrap()
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
    let vec = read_input("input");
    time(Task::One, task_one, vec.clone());
    time(Task::Two, task_two, vec);
}

fn task_one(mut mat: Matrix) -> i32
{
    let mut res = 0;
    for _ in 0..100
    {
        for i in 0..(mat.vec.len() as i32)
        {
            let x = i % 10;
            let y = i / 10;

            mat[[x, y]].energy += 1;
            if mat[[x, y]].energy > 9 && !mat[[x, y]].has_glowed
            {
                flash(&mut mat, x, y, &mut res);
            }
        }

        for i in 0..(mat.vec.len() as i32)
        {
            let x = i % 10;
            let y = i / 10;

            if mat[[x, y]].energy > 9
            {
                mat[[x, y]].energy = 0;
            }
            mat[[x, y]].has_glowed = false;
        }
    }
    res
}

fn task_two(mut mat: Matrix) -> i32
{
    let mut res = 0;
    for step in 1..
    {
        for i in 0..(mat.vec.len() as i32)
        {
            let x = i % 10;
            let y = i / 10;

            mat[[x, y]].energy += 1;
            if mat[[x, y]].energy > 9 && !mat[[x, y]].has_glowed
            {
                flash(&mut mat, x, y, &mut res);
            }
        }

        if mat.vec.iter().all(|octopus| octopus.has_glowed)
        {
            return step;
        }

        for i in 0..(mat.vec.len() as i32)
        {
            let x = i % 10;
            let y = i / 10;

            if mat[[x, y]].energy > 9
            {
                mat[[x, y]].energy = 0;
            }

            mat[[x, y]].has_glowed = false;
        }
    }
    unreachable!()
}

fn flash(mat: &mut Matrix, x: i32, y: i32, res: &mut i32)
{
    mat[[x, y]].has_glowed = true;
    *res += 1;

    for i in -1..=1
    {
        for j in -1..=1
        {
            let x = x + i;
            let y = y + j;

            if (0..10).contains(&x) && (0..10).contains(&y) && !(i == 0 && j == 0)
            {
                mat[[x, y]].energy += 1;
                if mat[[x, y]].energy > 9 && !mat[[x, y]].has_glowed
                {
                    flash(mat, x, y, res);
                }
            }
        }
    }
}
