use std::collections::*;

fn read_input<P>(path: P) -> Vec<String>
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
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

// Math go brrr
// https://math.stackexchange.com/questions/3157030/parametrizing-the-square-spiral
fn func(n: i32) -> (i32, i32)
{
    let n = n - 1;
    let n_root = (n as f32).sqrt().floor() as i32;
    let nh = if n_root % 2 == 0 { n_root } else { n_root - 1};
    let nh2 = nh*nh;

    if nh2 <= n && n <= nh2 + nh 
    {
        return (-(nh / 2) + n - nh2, nh / 2)
    }
    else if nh2 + nh < n && n <= nh2 + 2 * nh + 1
    {
        return (nh / 2, nh / 2 - n + nh2 + nh)
    }
    else if nh2 + 2 * nh + 1 < n && n <= nh2 + 3 * nh + 2 
    {
        return (nh / 2 - n + nh2 + 2 * nh + 1, -nh / 2 -1)
    }
    else if nh2 + 3 * nh + 2 < n && n <= nh2 + 4 * nh + 3
    {
        return (-nh/2-1, -nh / 2 - 1 + n - nh2 - 3 * nh - 2)
    }
    unreachable!()
}

fn manhattan_distance(p: (i32, i32)) -> i32 
{
    p.0.abs() + p.1.abs()
}

fn task_one(input: &[String]) -> i32
{
    let num: i32 = input[0].parse().unwrap();
    manhattan_distance(func(num))
}

fn point_add(p1: (i32, i32), p2: (i32, i32)) -> (i32, i32)
{
    (p1.0 + p2.0, p1.1 + p2.1)
}

fn neighbors(p: &(i32, i32), map: &HashMap<(i32, i32), i32>) -> i32
{
    let mut sum = 0;
    for y in -1..=1
    {
        for x in -1..=1
        {
            if y == 0 && x == 0 { continue; }
            let p = point_add(*p, (x, y));
            sum += *map.get(&p).unwrap_or(&0);
        }
    }
    sum
}

fn task_two(input: &[String]) -> i32
{
    let input_number: i32 = input[0].parse().unwrap();
    let dirs = [(1, 0), (0, -1), (-1, 0), (0, 1)];
    let mut map = HashMap::new();

    let mut curr = (0, 0);
    map.insert(curr, 1);

    for n in 1..
    {
        // idx cycles between 0 and 2
        let idx = 2 * (n % 2);
        for i in 0..=1
        {
            for _ in 0..n
            {
                curr = point_add(curr, dirs[idx + i]);
                let num = neighbors(&curr, &map);
                if num > input_number
                {
                    return num;
                }
                map.insert(curr, num);
            }
        }
    }
    unreachable!()
}
