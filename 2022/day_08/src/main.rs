fn create_map(input: &[String]) -> Vec<Vec<u32>>
{
    input
        .iter()
        .map(|line| line.chars().map(move |ch| ch.to_digit(10).unwrap()).collect())
        .collect()
}

fn count(tree: u32, map: &[Vec<u32>], iter: impl Iterator<Item = (usize, usize)>) -> usize
{
    let mut c = 0;
    for (x, y) in iter
    {
        c += 1;
        if map[y][x] >= tree
        {
            return c;
        }
    }
    c
}

fn task_one(input: &[String]) -> usize
{
    let map = create_map(input);

    let len_x = input[0].len();
    let len_y = input.len();

    let mut count = 0;

    for y in 0..len_y
    {
        for x in 0..len_x
        {
            let tree = map[y][x];

            if (0..x).all(|x| map[y][x] < tree)
                || (x + 1..len_x).all(|x| map[y][x] < tree)
                || (0..y).all(|y| map[y][x] < tree)
                || (y + 1..len_y).all(|y| map[y][x] < tree)
            {
                count += 1
            }
        }
    }
    count
}

fn task_two(input: &[String]) -> usize
{
    let len_x = input[0].len();
    let len_y = input.len();

    let mut max = 0;
    let map = create_map(input);

    for y in 0..len_y
    {
        for x in 0..len_x
        {
            let tree = map[y][x];
            let val = [
                count(tree, &map, (0..x).rev().map(|x| (x, y))),
                count(tree, &map, (x + 1..len_x).map(|x| (x, y))),
                count(tree, &map, (0..y).rev().map(|y| (x, y))),
                count(tree, &map, (y + 1..len_y).map(|y| (x, y))),
            ]
            .into_iter()
            .product::<usize>();

            max = std::cmp::max(max, val);
        }
    }
    max
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
