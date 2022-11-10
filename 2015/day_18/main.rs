use std::collections::*;

fn neighbors(p: (isize, isize), map: &HashMap<(isize, isize), bool>) -> usize
{
    let mut sum = 0;
    for y in -1..=1
    {
        for x in -1..=1
        {
            let xx = p.0 + x;
            let yy = p.1 + y;
            if (x == 0 && y == 0) || !(0..100).contains(&xx) || !(0..100).contains(&yy)
            {
                continue;
            }

            if map[&(xx, yy)]
            {
                sum += 1;
            }
        }
    }
    sum
}

fn task_one(input: &[String]) -> usize
{
    let mut map = HashMap::with_capacity(100 * 100);
    for (y, line) in input.iter().enumerate()
    {
        for (x, ch) in line.chars().enumerate()
        {
            map.insert((x as isize, y as isize), ch == '#');
        }
    }

    for _ in 0..100
    {
        let mut new = HashMap::with_capacity(100 * 100);
        for y in 0_isize..100
        {
            for x in 0_isize..100
            {
                let on = *map.get(&(x, y)).unwrap();
                let count = neighbors((x, y), &map);

                if on
                {
                    new.insert((x, y), count == 2 || count == 3);
                }
                else
                {
                    new.insert((x, y), count == 3);
                }
            }
        }
        map = new;
    }

    map.into_values().filter(|b| *b).count()
}

fn task_two(input: &[String]) -> usize
{
    let mut map = HashMap::with_capacity(100 * 100);
    for (y, line) in input.iter().enumerate()
    {
        for (x, ch) in line.chars().enumerate()
        {
            map.insert((x as isize, y as isize), ch == '#');
        }
    }

    let ons = [(0, 0), (0, 99), (99, 0), (99, 99)];
    for t in ons.iter()
    {
        map.insert(*t, true);
    }

    for _ in 0..100
    {
        let mut new = HashMap::with_capacity(100 * 100);

        for t in ons.iter()
        {
            new.insert(*t, true);
        }

        for y in 0_isize..100
        {
            for x in 0_isize..100
            {
                if ons.contains(&(x, y))
                {
                    continue;
                }
                let on = *map.get(&(x, y)).unwrap();
                let count = neighbors((x, y), &map);

                if on
                {
                    new.insert((x, y), count == 2 || count == 3);
                }
                else
                {
                    new.insert((x, y), count == 3);
                }
            }
        }
        map = new;
    }

    map.into_values().filter(|b| *b).count()
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
