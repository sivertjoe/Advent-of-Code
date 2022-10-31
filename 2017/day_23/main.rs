use std::collections::*;

#[derive(Clone, Debug)]
enum Value
{
    Number(i64),
    Reg(char),
}

impl Value
{
    fn get(&self, regs: &HashMap<char, i64>) -> i64
    {
        match self
        {
            Value::Number(n) => *n,
            Value::Reg(c) => *regs.get(c).unwrap_or(&0),
        }
    }
}

#[derive(Clone, Debug)]
enum Instruction
{
    Set(char, Value),
    Sub(char, Value),
    Mul(char, Value),
    Jnz(Value, Value),
}


fn parse(input: &[String]) -> Vec<Instruction>
{
    let to_char = |s: &str| s.chars().next().unwrap();

    let to_val =
        |s: &str| s.parse::<i64>().map(Value::Number).unwrap_or_else(|_| Value::Reg(to_char(s)));

    input
        .iter()
        .map(|line| match line.split_whitespace().collect::<Vec<&str>>().as_slice()
        {
            ["jnz", x, y] => Instruction::Jnz(to_val(x), to_val(y)),
            ["set", x, y] => Instruction::Set(to_char(x), to_val(y)),
            ["mul", x, y] => Instruction::Mul(to_char(x), to_val(y)),
            ["sub", x, y] => Instruction::Sub(to_char(x), to_val(y)),

            _ => unreachable!(),
        })
        .collect()
}


fn task_one(input: &[String]) -> usize
{
    let mut ip = 0;
    let instructions = parse(input);
    let mut registers = HashMap::new();

    let mut mul = 0;
    while let Some(ins) = instructions.get(ip)
    {
        use Instruction::*;
        match ins
        {
            Set(reg, val) =>
            {
                let val = val.get(&registers);
                let reg = registers.entry(*reg).or_insert(0);
                *reg = val;
                ip += 1;
            },
            Sub(reg, val) =>
            {
                *registers.entry(*reg).or_insert(0) -= val.get(&registers);
                ip += 1;
            },
            Mul(reg, val) =>
            {
                mul += 1;
                *registers.entry(*reg).or_insert(0) *= val.get(&registers);
                ip += 1;
            },
            Jnz(x, y) =>
            {
                let val = y.get(&registers);
                let val2 = x.get(&registers);
                if val2 != 0
                {
                    ip = (ip as i64 + val) as usize;
                }
                else
                {
                    ip += 1;
                }
            },
        }
    }
    mul
}

fn task_two(_input: &[String]) -> usize
{
    // The first lines in the input file
    let b = 81 * 100 + 100_000;
    let c = b + 17_000;

    let is_prime = |b: i32| {
        for d in 2..=(b as f32).sqrt() as i32
        {
            if b % d == 0
            {
                return false;
            }
        }
        true
    };
    (b..=c).step_by(17).filter(|v| !is_prime(*v)).count()
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
