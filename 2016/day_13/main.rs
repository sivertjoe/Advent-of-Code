use std::collections::*;

fn is_open((x, y): (usize, usize), num: usize) -> bool
{
    let v = x * x + 3 * x + 2 * x * y + y + y * y;
    let v = v + num;
    v.count_ones() % 2 == 0
}
use std::cmp::Reverse;

fn neighbors((x, y): (usize, usize), num: usize) -> impl Iterator<Item = (usize, usize)>
{
    let x = x as isize;
    let y = y as isize;
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        .into_iter()
        .filter(move |(x, y)| *x >= 0 && *y >= 0 && is_open((*x as usize, *y as usize), num))
        .map(|(x, y)| (x as usize, y as usize))
}

fn task_one(input: &[String]) -> usize
{
    let num = input[0].parse::<usize>().unwrap();
    let mut seen = HashSet::new();

    let mut vec = VecDeque::new();
    vec.push_back((Reverse(0), (1, 1)));
    seen.insert((1, 1));

    while let Some((cost, pos)) = vec.pop_front()
    {
        if pos == (31, 39)
        {
            return cost.0;
        }
        for neighbor in neighbors(pos, num)
        {
            if seen.insert(neighbor)
            {
                vec.push_back((Reverse(cost.0 + 1), neighbor));
            }
        }
    }
    unreachable!()
}

fn task_two(input: &[String]) -> usize
{
    let num = input[0].parse::<usize>().unwrap();

    let mut heap = BinaryHeap::new();
    heap.push((Reverse(0), (1, 1)));

    let mut dist: HashMap<(usize, usize), usize> = HashMap::new();
    dist.insert((1, 1), 0);

    while let Some((cost, pos)) = heap.pop()
    {
        for neighbor in neighbors(pos, num)
        {
            let next = (Reverse(cost.0 + 1), neighbor);
            let entry = dist.entry(neighbor).or_insert(usize::MAX);
            if next.0 .0 < *entry
            {
                *entry = next.0 .0;
                heap.push(next);
            }
        }
    }
    dist.into_values().filter(|v| *v <= 50).count()
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
