fn inc(password: &str) -> String
{
    let mut new = String::new();

    let mut carry = Some(());
    for ch in password.chars().rev()
    {
        let mut ch = ch;
        if carry.is_some() && ch == 'z'
        {
            ch = 'a';
        }
        else if carry.is_some() && ch != 'z'
        {
            ch = (ch as u8 + 1) as char;
            carry = None;
        }

        new.push(ch);
    }

    new.chars().rev().collect()
}

fn valid_password(password: &str) -> bool
{
    let increasing = || {
        password
            .as_bytes()
            .windows(3)
            .any(|arr| arr[2] == arr[1] + 1 && arr[2] == arr[0] + 2)
    };

    let no_iol = || {
        let bad = ['i', 'o', 'l'];
        password.chars().all(|ch| !bad.contains(&ch))
    };

    let overlapping = || {
        if let Some(arr) = password.as_bytes().windows(2).find(|arr| arr[0] == arr[1])
        {
            let ch = arr[0];
            password.as_bytes().windows(2).any(|arr| arr[0] != ch && arr[0] == arr[1])
        }
        else
        {
            false
        }
    };

    increasing() && no_iol() && overlapping()
}

struct PasswordIter
{
    password: String,
}

impl PasswordIter
{
    fn new(pswd: &str) -> Self
    {
        Self {
            password: pswd.to_owned()
        }
    }
}

impl Iterator for PasswordIter
{
    type Item = String;

    fn next(&mut self) -> Option<Self::Item>
    {
        let mut next = inc(&self.password);
        while !valid_password(&next)
        {
            next = inc(&next);
        }
        self.password = next.clone();
        Some(next)
    }
}

fn task_one(input: &[String]) -> String
{
    PasswordIter::new(&input[0]).into_iter().next().unwrap()
}

fn task_two(input: &[String]) -> String
{
    PasswordIter::new(&input[0]).into_iter().nth(1).unwrap()
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
