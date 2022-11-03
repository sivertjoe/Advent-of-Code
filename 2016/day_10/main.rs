use std::collections::*;

#[derive(Debug)]
enum Value
{
    Low,
    High,
}

#[derive(Debug)]
enum Give
{
    Bot,
    Output,
    Input,
}

#[derive(Default, Debug)]
struct Bot
{
    items:        Vec<i32>,
    instructions: VecDeque<(Value, Give, i32)>,
}

fn parse(input: &[String]) -> HashMap<i32, Bot>
{
    let mut map: HashMap<i32, Bot> = HashMap::new();
    for line in input
    {
        if line.starts_with("value")
        {
            let mut iter = line.split_whitespace();
            let val = iter.nth(1).unwrap().parse::<i32>().unwrap();
            let bot = iter.nth(3).unwrap().parse::<i32>().unwrap();
            map.entry(bot).or_default().items.push(val);
        }
        else
        {
            let mut iter = line.split_whitespace();
            //
            // 0   1   0    1  0   1  0  0   1   0   1  0
            // bot 2 gives low to bot 1 and high to bot 0
            //

            let bot = iter.nth(1).unwrap().parse::<i32>().unwrap();


            let bot = map.entry(bot).or_default();
            let value = match iter.nth(1).unwrap()
            {
                "low" => Value::Low,
                "high" => Value::High,
                _ => unreachable!(),
            };


            let out1 = match iter.nth(1).unwrap()
            {
                "output" => Give::Output,
                "input" => Give::Input,
                "bot" => Give::Bot,
                _ => unreachable!(),
            };

            let num1 = iter.nth(0).unwrap().parse().unwrap();
            bot.instructions.push_back((value, out1, num1));

            let value = match iter.nth(1).unwrap()
            {
                "low" => Value::Low,
                "high" => Value::High,
                _ => unreachable!(),
            };

            let out1 = match iter.nth(1).unwrap()
            {
                "output" => Give::Output,
                "input" => Give::Input,
                "bot" => Give::Bot,
                _ => unreachable!(),
            };

            let num1 = iter.nth(0).unwrap().parse().unwrap();
            bot.instructions.push_back((value, out1, num1));
        }
    }
    map
}

fn solve(input: &[String], early_exit: bool) -> i32
{
    let mut bots = parse(input);
    let mut output = HashMap::new();
    let mut input = HashMap::new();

    let mut flag = true;

    let keys: Vec<i32> = bots.keys().copied().collect();

    while flag
    {
        flag = false;
        for i in keys.iter().copied()
        {
            if bots[&i].items.len() == 2
            {
                flag = true;
                if let (Some(ins1), Some(ins2)) = (
                    bots.get_mut(&i).unwrap().instructions.pop_front(),
                    bots.get_mut(&i).unwrap().instructions.pop_front(),
                )
                {
                    let mut execute = |ins: (Value, Give, i32)| {
                        let vals = bots.get(&i).unwrap().items.clone();
                        let (a, b) = (vals[0], vals[1]);
                        if ((a == 61 && b == 17) || (a == 17 && b == 61)) && early_exit
                        {
                            return true;
                        }
                        let val = match ins.0
                        {
                            Value::Low => std::cmp::min(a, b),
                            Value::High => std::cmp::max(a, b),
                        };
                        match ins
                        {
                            (_, Give::Output, num) =>
                            {
                                output.insert(num, val);
                            },
                            (_, Give::Input, num) =>
                            {
                                input.insert(num, val);
                            },
                            (_, Give::Bot, num) =>
                            {
                                bots.get_mut(&num).unwrap().items.push(val);
                            },
                        };
                        false
                    };
                    if execute(ins1) || execute(ins2)
                    {
                        return i as i32;
                    }
                    bots.get_mut(&i).unwrap().items.clear();
                }
            }
        }
    }


    output[&0] * output[&1] * output[&2]
}

fn task_one(input: &[String]) -> i32
{
    solve(input, true)
}

fn task_two(input: &[String]) -> i32
{
    solve(input, false)
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
