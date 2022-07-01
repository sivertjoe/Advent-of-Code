fn read_input<P>(path: P) -> (i64, i64, i64, i64)
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    let line = std::io::BufReader::new(file).lines().flatten().next().unwrap();

    let fst = line.find('=').unwrap() + 1;
    let snd = line.find(',').unwrap();

    let x = &line[fst..snd];

    let fst = line.rfind('=').unwrap() + 1;
    let y = &line[fst..];

    let unpack_tuples = |s: &str| {
        s.split_once("..")
            .map(|(s1, s2)| (s1.parse::<i64>().unwrap(), s2.parse::<i64>().unwrap()))
            .unwrap()
    };


    let (x1, x2) = unpack_tuples(x);
    let (y1, y2) = unpack_tuples(y);

    (x1, x2, y1, y2)
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


fn task_one(input: &(i64, i64, i64, i64)) -> i64
{
    /* Given a starting speed of v
     * at y = 0, the y velocity y_v = -(v + 1).
     * If y_v < y_lower_bound, we will
     * overshoot,thus, the highest speed is when
     * y_v (at y = 0) = y_lower_bound, which occurs
     * at starting speed one less than the y_lower_bound
     */
    let speed = input.2.abs() - 1;
    speed * (speed + 1) / 2
}

fn task_two(input: &(i64, i64, i64, i64)) -> i64
{
    let mut res = 0;
    for x0 in 0..=input.1
    {
        for y0 in input.2..(-input.2)
        {
            simulate(x0, y0, &mut res, input);
        }
    }
    res
}

fn simulate(vx0: i64, vy0: i64, res: &mut i64, input: &(i64, i64, i64, i64))
{
    let (mut x, mut y) = (0, 0);
    let (mut vx, mut vy) = (vx0, vy0);

    loop
    {
        x += vx;
        y += vy;

        if vx > 0
        {
            vx -= 1;
        }

        vy -= 1;

        if x >= input.0 && x <= input.1 && y >= input.2 && y <= input.3
        {
            *res += 1;
            return;
        }

        if x > input.1 || y < input.2
        {
            return;
        }
    }
}
