fn read_input<T, P>(path: P) -> Vec<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    std::io::BufReader::new(file)
        .lines()
        .flatten()
        .map(|line| line.split(',').map(|n| n.parse::<T>().unwrap()).collect::<Vec<_>>())
        .flatten()
        .collect()
}

fn time<F, T, U>(pre: &'static str, f: F, arg: T)
where
    F: Fn(T) -> U,
    U: std::fmt::Display,
{
    let t0 = std::time::Instant::now();
    let res = f(arg);
    let t1 = std::time::Instant::now();
    println!("Task {}: {}\t({}ms)", pre, res, t1.duration_since(t0).as_millis());
}

fn main()
{
    let mut vec = read_input("input");
    vec.sort();
    time("one", task_one, &vec);
    time("two", task_two, &vec);
}

fn task_one(vec: &[i32]) -> i32
{
    let median = vec[vec.len() / 2];
    vec.into_iter().map(|n| (n - median).abs()).sum()
}

fn task_two(vec: &[i32]) -> i32
{
    let mean = (vec.iter().sum::<i32>() as f32 / vec.len() as f32);

    let calc = |n: i32, z: i32| {
        let n = (n - z).abs();
        (n * (n + 1)) / 2
    };

    let calc_fuel_consumption = |z: i32| vec.iter().map(|n| calc(*n, z)).sum();

    std::cmp::min(
        calc_fuel_consumption(mean.floor() as i32),
        calc_fuel_consumption(mean.ceil() as i32),
    )
}
