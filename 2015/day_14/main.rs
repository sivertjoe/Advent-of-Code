fn parse_line(line: &String) -> (usize, usize, usize)
{
    let mut iter = line.split_whitespace();
    let fst = iter.nth(3).unwrap().parse().unwrap();
    let snd = iter.nth(2).unwrap().parse().unwrap();
    let thrd = iter.nth(6).unwrap().parse().unwrap();
    (fst, snd, thrd)
}

fn max_distance((speed, time, rest): (usize, usize, usize), total_time: usize) -> usize
{
    let mut dist = 0;
    let mut count = 0;
    while count < total_time
    {
        let left = total_time - count;
        if left < time
        {
            dist += speed * left;
            break;
        }
        dist += speed * time;
        count += rest + time;
    }
    dist
}

fn task_one(input: &[String]) -> usize
{
    let total_time = 2503;
    let dist = |tuple| max_distance(tuple, total_time);
    input.iter().map(parse_line).map(dist).max().unwrap()
}

fn task_two(input: &[String]) -> usize
{
    let total_time = 2503;

    let mut deers = input
        .iter()
        .map(|line| {
            let t = parse_line(line);
            (t.1, t.2, t.0, t.1, t.2)
        })
        .collect::<Vec<_>>();

    let mut dist = (0..deers.len()).map(|_| 0).collect::<Vec<_>>();
    let mut points = (0..deers.len()).map(|_| 0).collect::<Vec<_>>();

    for _ in 0..total_time
    {
        // step-left, rest-left speed, time, rest
        for (i, deer) in deers.iter_mut().enumerate()
        {
            if deer.0 == 0
            {
                deer.1 -= 1;
                if deer.1 == 0
                {
                    deer.0 = deer.3;
                    deer.1 = deer.4;
                }
            }
            else
            {
                deer.0 -= 1;
                dist[i] += deer.2;
            }
        }
        let max = *dist.iter().max().unwrap();
        for (i, d) in dist.iter().enumerate()
        {
            if *d == max
            {
                points[i] += 1;
            }
        }
    }
    points.into_iter().max().unwrap()
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
