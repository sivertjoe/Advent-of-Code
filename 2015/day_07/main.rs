use std::collections::*;

#[derive(Clone, Debug)]
enum Value
{
    Number(i64),
    Reg(String),
}

impl Value
{
    fn get(&self, regs: &HashMap<String, i64>) -> Option<i64>
    {
        match self
        {
            Value::Number(n) => Some(*n),
            Value::Reg(c) => regs.get(c).cloned(),
        }
    }
}

#[derive(Clone, Debug)]
enum Instruction
{
    Assign(Value, String),
    And(Value, Value, String),
    Or(Value, Value, String),
    LShift(Value, Value, String),
    RShift(Value, Value, String),
    Not(Value, String),
}


fn parse(input: &[String]) -> Vec<Instruction>
{
    let to_reg = |s: &str| s.to_string();

    let to_val =
        |s: &str| s.parse::<i64>().map(Value::Number).unwrap_or_else(|_| Value::Reg(to_reg(s)));

    use Instruction::*;
    input
        .iter()
        .map(|line| match line.split_whitespace().collect::<Vec<&str>>().as_slice()
        {
            [num, "->", reg] => Assign(to_val(num), to_reg(reg)),
            [x, "AND", y, "->", dst] => And(to_val(x), to_val(y), to_reg(dst)),
            [x, "OR", y, "->", dst] => Or(to_val(x), to_val(y), to_reg(dst)),
            [x, "LSHIFT", y, "->", dst] => LShift(to_val(x), to_val(y), to_reg(dst)),
            [x, "RSHIFT", y, "->", dst] => RShift(to_val(x), to_val(y), to_reg(dst)),
            ["NOT", x, "->", dst] => Not(to_val(x), to_reg(dst)),

            _ => unreachable!(),
        })
        .collect()
}

fn run(ins: Vec<Instruction>) -> HashMap<String, i64>
{
    let mut regs = HashMap::new();
    let mut ins = ins;

    while !ins.is_empty()
    {
        use Instruction::*;
        ins.retain(|r#in| {
            match r#in
            {
                Assign(v, dst) =>
                {
                    let Some(val) = v.get(&regs) else { return true; };
                    regs.insert(dst.clone(), val);
                },
                And(x, y, dst) =>
                {
                    let (Some(x), Some(y)) = (x.get(&regs), y.get(&regs)) else {return true; };
                    regs.insert(dst.clone(), x & y);
                },
                Or(x, y, dst) =>
                {
                    let (Some(x), Some(y)) = (x.get(&regs), y.get(&regs)) else {return true; };
                    regs.insert(dst.clone(), x | y);
                },
                LShift(x, y, dst) =>
                {
                    let (Some(x), Some(y)) = (x.get(&regs), y.get(&regs)) else {return true; };
                    regs.insert(dst.clone(), x << y);
                },
                RShift(x, y, dst) =>
                {
                    let (Some(x), Some(y)) = (x.get(&regs), y.get(&regs)) else {return true; };
                    regs.insert(dst.clone(), x >> y);
                },
                Not(x, dst) =>
                {
                    let Some(val) = x.get(&regs) else { return true; };
                    regs.insert(dst.clone(), !val);
                },
            }
            false
        });
    }
    regs
}

fn task_one(input: &[String]) -> i64
{
    let ins = parse(input);
    let regs = run(ins);
    regs[&'a'.to_string()]
}

fn task_two(input: &[String]) -> i64
{
    let ins = parse(input);
    let regs = run(ins);
    let v = regs[&'a'.to_string()];

    let mut ins = parse(input);
    let r#in = ins
        .iter_mut()
        .find(|r#in| matches!(r#in, Instruction::Assign(_, dst) if dst.starts_with('b')))
        .unwrap();
    *r#in = Instruction::Assign(Value::Number(v), "b".to_owned());
    let regs = run(ins);
    regs[&'a'.to_string()]
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
