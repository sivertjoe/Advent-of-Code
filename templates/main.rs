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
        .map(|line| line.parse::<T>().unwrap())
        .collect()
}

fn time<F, T, U>(pre: &'static str, f: F, arg: T)
where
    F: Fn(T) -> U,
    U: std::fmt::Display,
{
    let t = std::time::Instant::now();
    let res = f(arg);
    println!("({}ms) \tTask {}: {}", t.elapsed().as_millis(), pre, res);
}

fn main()
{
    let vec = read_input("input");
    time("one", task_one, &vec);
    time("two", task_two, &vec);
}

fn task_one(vec: &[i32]) -> i32
{
    unimplemented!()
}

fn task_two(vec: &[i32]) -> i32
{
    unimplemented!()
}
