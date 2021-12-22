struct Line
{
    patterns: Vec<String>,
    output:   Vec<String>,
}

impl std::str::FromStr for Line
{
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err>
    {
        let map = |s: &str| s.split(' ').map(|c| c.to_string()).collect::<Vec<_>>();
        let (fst, snd) = s.split_once(" | ").unwrap();


        Ok(Self {
            patterns: map(fst), output: map(snd)
        })
    }
}

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

fn task_one(vec: &[Line]) -> i32
{
    vec.iter()
        .map(|line| &line.output)
        .flatten()
        .filter(|s| s.len() != 6 && s.len() != 5)
        .count() as i32
}

fn task_two(vec: &[Line]) -> i32
{
    let mut res = 0;

    for line in vec
    {
        let mut occ: [u8; 7] = [0; 7];
        let idx = |c: char| (c as u8 - b'a') as usize;

        for line in &line.patterns
        {
            for c in line.chars()
            {
                occ[idx(c)] += 1;
            }
        }

        let mut m = 1000;
        res += line.output.iter().fold(0, |acc, number| {
            let sum = number.chars().map(|c| occ[idx(c)]).sum::<u8>() as i32;
            let val = translate(sum);
            let f = acc + val * m;
            m /= 10;
            f
        });
    }
    res
}


/*
 * Since we know that the pattern line, e.g., "ab acedgfb cdfbe gcdfa
 * fbcad..." contains the number 1-9, we know that the segment 'c', for
 * instance, occur 8 times: 0, 1, 2, 3, 4, 7, 8, 9 and the segment 'f' occur
 * 9 times: 0, 1, 3, 4, 5, 6, 7, 8, 9; for all segments.
 * * Therefore, if we sum the letters using the occurrences, we can figure
 * out which number is which. For example, the number 1 has segments 'c' and
 * 'f'. The sum is 17. Since numbers are unique, this sum is also unique.
 * This mapping can be done for each number. The table below is the mapping
 */
#[inline]
fn translate(sum: i32) -> i32
{
    match sum
    {
        42 => 0,
        17 => 1,
        34 => 2,
        39 => 3,
        30 => 4,
        37 => 5,
        41 => 6,
        25 => 7,
        49 => 8,
        45 => 9,
        _ => unreachable!(),
    }
}
