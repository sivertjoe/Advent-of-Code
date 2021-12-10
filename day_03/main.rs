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

fn task_one(vec: &[String]) -> i32
{
    let n = vec[0].len();
    let gamma = create_mask(vec, &|a, b| a >= b);

    let mask = (1 << n) - 1;
    (gamma * (!gamma & mask)) as i32
}

fn task_two(vec: &[String]) -> i32
{
    let ogr = calc(vec, |a, b| a >= b);
    let csr = calc(vec, |a, b| a < b);
    ogr * csr
}

fn create_mask<F>(vec: &[String], f: &F) -> usize
where
    F: Fn(usize, usize) -> bool,
{
    let n = vec[0].len();
    let mut num = vec![0; n];

    for v in vec
    {
        for (i, bit) in v.chars().enumerate()
        {
            num[i] += bit.to_digit(10).unwrap() as usize;
        }
    }


    num.into_iter()
        .zip((0..n).rev()) // enumerate in rev, e.g, N-1, N-2, ..., 0
        .fold(0, |acc, (n, i)| acc | (f(n, vec.len() - n) as usize) << i)
}

fn get_bit<F>(vec: &[String], f: &F, i: usize) -> u32
where
    F: Fn(usize, usize) -> bool,
{
    let mut num = 0;

    for line in vec
    {
        num += ((line.as_bytes()[i] as char) == '1') as usize;
    }

    f(num, vec.len() - num) as u32
}


fn calc<F>(vec: &[String], f: F) -> i32
where
    F: Fn(usize, usize) -> bool,
{
    let mut vec = vec.to_vec();
    let n = vec[0].len();

    for i in 0..n
    {
        let bit = get_bit(&vec, &f, i);
        vec.retain(|line| (line.as_bytes()[i] as char).to_digit(10).unwrap() == bit);
        if vec.len() == 1
        {
            break;
        }
    }
    i32::from_str_radix(vec[0].as_str(), 2).unwrap()
}
