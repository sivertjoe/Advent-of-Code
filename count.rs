use std::{path::Path, process::Command};

fn build(path: &str)
{
    let p = format!("./{}/compile.sh", &path);
    let p = std::path::Path::new(&p);
    let root = p.parent().unwrap().parent().unwrap().to_str().unwrap();
    
    Command::new(format!("./{}/compile.sh", root))
        .arg(path)
        .output()
        .unwrap();
}

fn get_num(s: &str) -> Option<i32>
{
    let l = s.find('(')? + 1;
    let r = s.find(')')?;

    s[l..r - 2].parse::<i32>().ok()
}

fn run(path: &str) -> (i32, i32)
{
    let f = Command::new("./main").arg("input").current_dir(path).output().unwrap();
    let out = std::str::from_utf8(&f.stdout).unwrap();
    assert!(f.status.success());

    out.split_once('\n')
        .map(|(a, b)| (get_num(a).unwrap(), get_num(b).unwrap_or(-1)))
        .unwrap()
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
    let folder = std::env::args().nth(1).expect("Usage: ./count <year path>");


    let mut silver = Vec::new();
    let mut gold = Vec::new();
    let mut not_done = Vec::new();

    for day in 1..=25
    {
        let day_text = if day < 10 { format!("0{}", day) } else { format!("{}", day) };

        let folder = format!("{}/day_{}", folder, day_text);
        let path = Path::new(&folder);

        if path.exists()
        {
            let (s, g) = build_and_run(path);
            silver.push((day, s));
            if g != -1
            {
                gold.push((day, g));
            }
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
