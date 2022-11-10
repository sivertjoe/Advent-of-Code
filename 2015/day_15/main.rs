#[derive(Debug)]
struct Ingredient
{
    capacity:   i64,
    durability: i64,
    flavor:     i64,
    texture:    i64,
    calories:   i64,
}


fn parse(line: &String) -> Ingredient
{
    let mut iter = line.split_whitespace();
    let mut get = |n: usize| {
        let s = iter.nth(n).unwrap();
        s.trim_matches(',').parse::<i64>().unwrap()
    };

    Ingredient {
        capacity:   get(2),
        durability: get(1),
        flavor:     get(1),
        texture:    get(1),
        calories:   get(1),
    }
}

fn sum<const N: usize>(ingredients: &[Ingredient], v: [i64; N], cal_cap: bool) -> Option<i64>
{
    let mut cap = 0;
    let mut dur = 0;
    let mut fla = 0;
    let mut tex = 0;
    let mut cal = 0;

    for (i, ing) in ingredients.iter().enumerate()
    {
        cap += ing.capacity * v[i];
        dur += ing.durability * v[i];
        fla += ing.flavor * v[i];
        tex += ing.texture * v[i];
        cal += ing.calories * v[i];
    }

    let res = [cap, dur, fla, tex]
        .into_iter()
        .fold(1_i64, |acc, x| acc.saturating_mul(if x < 0 { 0 } else { x }));
    (!cal_cap || cal == 500).then_some(res)
}

fn solve(input: &[String], cal_cap: bool) -> i64
{
    let ingredients = input.iter().map(parse).collect::<Vec<_>>();
    let mut max = 0;
    for i in 1..100
    {
        for j in 1..100
        {
            for k in 1..100
            {
                for z in 1..100
                {
                    if i + j + k + z > 100
                    {
                        continue;
                    }
                    if let Some(sum) = sum(&ingredients, [i, j, k, z], cal_cap)
                    {
                        max = std::cmp::max(sum, max);
                    }
                }
            }
        }
    }

    max
}

fn task_one(input: &[String]) -> i64
{
    solve(input, false)
}

fn task_two(input: &[String]) -> i64
{
    solve(input, true)
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
