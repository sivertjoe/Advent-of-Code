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
        .map(|line| line.split(',').map(|n| n.parse::<T>().unwrap()).collect::<Vec<T>>())
        .flatten()
        .collect()
}

fn transform_input(vec: &[i32]) -> [usize; 9]
{
    let mut array: [usize; 9] = [0; 9];

    for val in vec
    {
        array[*val as usize] += 1;
    }
    array
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
    let vec = read_input("input");
    time("one", task_one, &vec);
    time("two", task_two, &vec);
}

fn task_one(vec: &[i32]) -> usize
{
    calculate_population_after_days(80, vec)
}

fn task_two(vec: &[i32]) -> usize
{
    calculate_population_after_days(256, vec)
}

fn calculate_population_after_days(n: usize, vec: &[i32]) -> usize
{
    let mut arr = transform_input(vec);

    for _ in 0..n
    {
        arr.rotate_left(1); // ['a', 'b', 'c'] -> ['b', 'c', 'a']
        arr[6] += arr[8];
    }

    arr.into_iter().sum()
}
