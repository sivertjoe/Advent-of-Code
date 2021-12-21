use std::collections::*;

fn read_input<P>(path: P) -> (i32, i32)
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    let mut lines = std::io::BufReader::new(file).lines().flatten();

    let p1 = lines.next().unwrap().rsplit_once(' ').unwrap().1.parse::<i32>().unwrap();
    let p2 = lines.next().unwrap().rsplit_once(' ').unwrap().1.parse::<i32>().unwrap();

    (p1, p2)
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

fn task_one((p1, p2): &(i32, i32)) -> i32
{
    fn _task_one(p1: i32, s1: i32, p2: i32, s2: i32, die: i32) -> i32
    {
        if s2 >= 1000
        {
            s1 * (die - 1)
        }
        else
        {
            let p = (p1 + (die * 3 + 3)) % 10;
            _task_one(p2, s2, p, s1 + p + 1, die + 3)
        }
    }

    _task_one(*p1 - 1, 0, *p2 - 1, 0, 1)
}

fn task_two((p1, p2): &(i32, i32)) -> i64
{
    let mut map = HashMap::new();
    let (w1, w2) = simulate((*p1, 0), (*p2, 0), &mut map);
    std::cmp::max(w1, w2)
}

fn simulate(
    (p1, p1s): (i32, i32),
    (p2, p2s): (i32, i32),
    map: &mut HashMap<(i32, i32, i32, i32), (i64, i64)>,
) -> (i64, i64)
{
    /*
     * Clarifications:
     * Originally this was just p % 10,
     * with subtracted p1 and p2, but, for whatever reason,
     * this is ~ 30% faster on my machine (13ms vs 19ms).
     * On godbolt.org, this version produces MORE assembly,
     * so idk _why_ exactly it is faster..
     * I guess something hardware specific 🤷‍♀️
     */
    let roll = |p| ((p - 1) % 10) + 1;

    if p1s >= 21
    {
        return (1, 0);
    }
    if p2s >= 21
    {
        return (0, 1);
    }

    // Position has been reached before
    if let Some((w1, w2)) = map.get(&(p1, p1s, p2, p2s))
    {
        return (*w1, *w2);
    }

    let mut ans = (0, 0);
    for d1 in 1..=3
    {
        for d2 in 1..=3
        {
            for d3 in 1..=3
            {
                let p1 = roll(p1 + d1 + d2 + d3);

                let (w2, w1) = simulate((p2, p2s), (p1, p1s + p1), map);

                ans.0 += w1;
                ans.1 += w2;
            }
        }
    }

    map.insert((p1, p1s, p2, p2s), ans);
    return ans;
}
