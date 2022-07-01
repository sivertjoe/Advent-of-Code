use std::collections::*;
fn read_input<P>(path: P) -> (String, HashMap<(char, char), char>)
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();


    let mut iter = std::io::BufReader::new(file).lines().flatten();

    let template = iter.next().unwrap();
    let pair = iter
        .skip(1)
        .map(|line| {
            line.split_once(" -> ")
                .map(|(a, b)| {
                    // Seperate AB to (A, B)
                    let (p1, p2) = a.split_at(1);
                    let (c1, c2) = (p1.parse::<char>().unwrap(), p2.parse::<char>().unwrap());
                    ((c1, c2), b.parse::<char>().unwrap())
                })
                .unwrap()
        })
        .collect::<HashMap<_, _>>();

    (template, pair)
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

fn main()
{
    let vec = read_input("input");
    time(Task::One, task_one, &vec);
    time(Task::Two, task_two, &vec);
}

fn solve(initial: &str, pair: &HashMap<(char, char), char>, n: usize) -> i64
{
    let mut res: [i64; 26] = [0; 26];
    let mut map = map_create(initial, &mut res);

    for _ in 0..n
    {
        map = map_next(map, pair, &mut res);
    }

    let max = res.iter().max().unwrap();
    let min = res.iter().filter(|c| **c != 0).min().unwrap();
    max - min
}

fn task_one((temp, pair): &(String, HashMap<(char, char), char>)) -> i64
{
    solve(temp, pair, 10)
}

fn task_two((temp, pair): &(String, HashMap<(char, char), char>)) -> i64
{
    solve(temp, pair, 40)
}

#[inline]
fn idx(c: char) -> usize
{
    (c as u8 - b'A') as usize
}

fn map_create(s: &str, count: &mut [i64; 26]) -> HashMap<(char, char), i64>
{
    let mut map = HashMap::with_capacity(s.len());
    let s: Vec<char> = s.chars().collect();

    count[idx(s[0])] += 1;
    for w in s.windows(2)
    {
        *map.entry((w[0], w[1])).or_insert(0) += 1;
        count[idx(w[1])] += 1;
    }

    map
}

fn map_next(
    map: HashMap<(char, char), i64>,
    pair: &HashMap<(char, char), char>,
    count: &mut [i64; 26],
) -> HashMap<(char, char), i64>
{
    /* With the rule: NN -> O:
     * the chars (N, N) will produce: {(N, O), (O, N)}.
     * The number of 'N' stays unchanged, but we have added one 'O'
     * and thus we increment the 'O'.
     */
    let mut new = HashMap::with_capacity(map.len() * 2);
    for ((c1, c2), v) in map
    {
        let c = pair[&(c1, c2)];

        *new.entry((c1, c)).or_insert(0) += v;
        *new.entry((c, c2)).or_insert(0) += v;
        count[idx(c)] += v;
    }

    new
}
