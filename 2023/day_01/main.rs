fn task_one(input: &[String]) -> usize
{
    let _iter = input
        .into_iter()
        .map(|line| {
            let s = line.chars().find(|c| c.is_digit(10)).unwrap();
            let c = line.chars().rev().find(|c| c.is_digit(10)).unwrap();

            format!("{s}{c}").parse::<usize>().unwrap()
        })
        .sum::<usize>();
    _iter
}

fn task_two(input: &[String]) -> usize
{
    let nums = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    input
        .into_iter()
        .map(|line| {
            let mut vec: Vec<(usize, usize)> = Vec::new();
            for (i, n) in nums.iter().enumerate()
            {
                /*if let Some(nn) = line.find(n)
                {
                    let a = i + 1;
                    vec.push((nn, a));
                }*/
                for (idx, _) in line.match_indices(n)
                {
                    let a = i + 1;
                    vec.push((idx, a));
                }
            }
            for (i, n) in line.chars().enumerate()
            {
                if n.is_digit(10)
                {
                    let a = n.to_string().parse::<usize>().unwrap();
                    vec.push((i, a));
                }
            }

            vec.sort_by_key(|k| k.0);
            let first = vec.first().unwrap_or(&(0, 0));
            let last = vec.last().unwrap_or(&(0, 0));

            let value = first.1 * 10 + last.1;
            //println!("{line} = {value}",);
            value
        })
        .sum()
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
