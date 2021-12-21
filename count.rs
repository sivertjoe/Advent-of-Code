use std::{path::Path, process::Command};

fn build(path: &str)
{
    let _f = Command::new("rustc")
        .args(vec!["-C", "opt-level=3", "--edition", "2021", "main.rs"])
        .current_dir(path)
        .output()
        .unwrap();

    // assert!(f.status.success());
}

fn get_num(s: &str) -> i32
{
    let l = s.find('(').unwrap() + 1;
    let r = s.find(')').unwrap();

    s[l..r - 2].parse().unwrap()
}

fn run(path: &str) -> (i32, i32)
{
    let f = Command::new("./main").arg("input").current_dir(path).output().unwrap();
    let out = std::str::from_utf8(&f.stdout).unwrap();
    assert!(f.status.success());

    out.split_once('\n').map(|(a, b)| (get_num(a), get_num(b))).unwrap()
}

fn clean(path: &str)
{
    let f = Command::new("rm").arg("main").current_dir(path).output().unwrap();
    assert!(f.status.success());
}

fn build_and_run(path: &Path) -> (i32, i32)
{
    let path = path.as_os_str().to_str().unwrap();

    build(path);

    let res = run(path);
    clean(path);

    res
}

fn main()
{
    let mut silver = Vec::new();
    let mut gold = Vec::new();
    let mut not_done = Vec::new();

    for day in 1..=25
    {
        let day_text = if day < 10 { format!("0{}", day) } else { format!("{}", day) };

        let folder = format!("day_{}", day_text);
        let path = Path::new(&folder);

        if path.exists()
        {
            let (s, g) = build_and_run(path);
            silver.push((day, s));
            gold.push((day, g));
        }
        else
        {
            not_done.push(day);
        }
    }

    if !not_done.is_empty()
    {
        let mut s = String::new();
        let mut first = true;
        for day in not_done
        {
            if !first
            {
                s.push_str(", ");
            }
            let s2 = format!("\x1b[0;33;31m{}\x1b[0m", day);
            s.push_str(s2.as_str());
            first = false;
        }
        println!("Days not completed: {}", s);
    }

    println!("STATS:\n");
    print_info(Task::Silver, &silver);
    print_info(Task::Gold, &gold);

    let total = gold.iter().chain(silver.iter()).map(|(_, time)| time).sum::<i32>();
    println!("\nTOTAL TIME: {}ms", total);
}


fn print_info(task: Task, vec: &[(i32, i32)])
{
    match task
    {
        Task::Silver => println!("\x1b[0;34;34m{}\x1b[0m:", "Silver"),
        Task::Gold => println!("\x1b[0;33;10m{}\x1b[0m:", "Gold"),
    };

    let mut _vec: Vec<_> = vec.iter().map(|(_, time)| *time).collect();
    _vec.sort();

    let median = _vec[_vec.len() / 2];

    let total = vec.iter().map(|(_, time)| time).sum::<i32>();
    let avg = total / vec.len() as i32;

    let (day, time) = vec.iter().max_by_key(|k| k.1).unwrap();

    println!("\t Total time:\t{}ms", total);
    println!("\t Average time:\t{}ms", avg);
    println!("\t Median time:\t{}ms", median);
    println!("\t Highest time:\t{}ms, day: {}", time, day);
}

enum Task
{
    Silver,
    Gold,
}
