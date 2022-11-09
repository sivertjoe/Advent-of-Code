use std::collections::*;

// * It contains at least three vowels (aeiou only), like aei, xazegov, or
//   aeiouaeiouaeiou.
// * It contains at least one letter that appears twice in a row, like xx,
//   abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
// * It does not contain the strings
// ab, cd, pq, or xy, even if they are part of one of the other requirements.


fn at_least_three_vowels(s: &str) -> bool
{
    let vowels = ['a', 'e', 'i', 'o', 'u'];
    let mut count = 0;
    for ch in s.chars()
    {
        if vowels.contains(&ch)
        {
            count += 1;
        }
        if count == 3
        {
            return true;
        }
    }
    false
}

fn at_least_same_letter_twice(s: &str) -> bool
{
    s.as_bytes().windows(2).any(|arr| arr[0] == arr[1])
}

fn not_contains_bad_str(s: &str) -> bool
{
    let bad = ["ab", "cd", "pq", "xy"].into_iter().map(|s| s.as_bytes()).collect::<Vec<_>>();
    !s.as_bytes().windows(2).any(|arr| bad.contains(&arr))
}

fn pair_of_two(s: &str) -> bool
{
    let mut map = HashMap::new();
    for (i, arr) in s.as_bytes().windows(2).enumerate()
    {
        if let Some(j) = map.insert((arr[0], arr[1]), i)
        {
            if i - j > 1
            {
                return true;
            }
        }
    }
    false
}

fn aba(s: &str) -> bool
{
    s.as_bytes().windows(3).any(|arr| arr[0] == arr[2])
}

fn task_one(input: &[String]) -> usize
{
    input
        .iter()
        .filter(|line| {
            at_least_three_vowels(line)
                && at_least_same_letter_twice(line)
                && not_contains_bad_str(line)
        })
        .count()
}


fn task_two(input: &[String]) -> usize
{
    input.iter().filter(|line| aba(line) && pair_of_two(line)).count()
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
