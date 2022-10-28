use std::collections::*;


#[derive(Debug, Clone, Copy)]
struct Node
{
    next: usize,
    val:  usize,
}

fn solve<const N: usize>(step: usize, target: usize) -> usize
{
    let mut vec: Vec<Node> = Vec::with_capacity(N);

    let node = Node {
        next: 0, val: 0
    };
    vec.push(node);


    let mut pos = 0;
    for elem in 1..=N
    {
        for _ in 0..step
        {
            pos = vec[pos].next;
        }

        let new = Node {
            val: elem, next: vec[pos].next
        };
        vec[pos].next = elem;
        vec.push(new);
        pos = elem;
    }

    let mut p = 0;
    let mut i = 0;
    for _ in 0..=N
    {
        if vec[p].val == target
        {
            let next = vec[p].next;
            return vec[next].val;
        }
        p = vec[p].next;
    }
    unreachable!()
}

fn task_one(input: &[String]) -> usize
{
    let step: usize = input[0].parse().unwrap();
    solve::<2017>(step, 2017)
}

fn task_two(input: &[String]) -> usize
{
    // Although solve<50000000>(step, 0)
    // will work, it is very slow, ~100s
    // This is far quicker.
    let step: usize = input[0].parse().unwrap();
    let mut after0 = 0;
    let mut pos = 0;
    let mut i = 1;

    while i <= 50000000
    {
        pos = (pos + step) % i + 1;
        if pos == 1
        {
            after0 = i;
        }
        let fits = (i - pos) / step;
        pos += fits * (step + 1);
        i += fits + 1;
    }
    after0
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
