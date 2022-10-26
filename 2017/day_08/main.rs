use std::collections::*;

fn get_operator(operator: &str) -> Box<dyn Fn(i32, i32) -> bool>
{
    match operator
    {
        "<" => Box::new(|a, b| a < b),
        "<=" => Box::new(|a, b| a <= b),
        ">" => Box::new(|a, b| a > b),
        ">=" => Box::new(|a, b| a >= b),
        "==" => Box::new(|a, b| a == b),
        "!=" => Box::new(|a, b| a != b),
        _ => unreachable!(),
    }
}

struct Instruction
{
    reg:  String,
    sign: i32,
    val:  i32,
    func: (String, i32, Box<dyn Fn(i32, i32) -> bool>),
}

fn parse(input: &[String]) -> Vec<Instruction>
{
    input
        .into_iter()
        .map(|line| {
            // rz inc -592 if m <= 1
            let mut iter = line.split_whitespace();

            let reg = iter.next().unwrap().to_string();
            let sign = if iter.next().unwrap() == "inc" { 1 } else { -1 };
            let val = iter.next().unwrap().parse::<i32>().unwrap();
            let _ = iter.next(); // skip 'if'
            let cmp_reg = iter.next().unwrap().to_string();
            let func = get_operator(iter.next().unwrap());
            let cmp_val = iter.next().unwrap().parse::<i32>().unwrap();

            Instruction {
                reg,
                sign,
                val,
                func: (cmp_reg, cmp_val, func),
            }
        })
        .collect()
}

fn execute_instruction(instruction: &Instruction, registers: &mut HashMap<String, i32>)
{
    let cmp_reg = registers.get(&instruction.func.0).unwrap_or(&0);
    if instruction.func.2(*cmp_reg, instruction.func.1)
    {
        *registers.entry(instruction.reg.clone()).or_insert(0) +=
            instruction.sign * instruction.val;
    }
}

fn task_one(input: &[String]) -> i32
{
    let ins = parse(input);
    let mut registers = HashMap::new();

    for instruction in ins
    {
        execute_instruction(&instruction, &mut registers);
    }

    *registers.values().max().unwrap()
}

fn task_two(input: &[String]) -> i32
{
    let mut max = 0;
    let ins = parse(input);
    let mut registers = HashMap::new();

    for instruction in ins
    {
        execute_instruction(&instruction, &mut registers);
        let reg_val = registers.get(&instruction.reg).unwrap_or(&0);
        max = std::cmp::max(*reg_val, max);
    }

    max
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
