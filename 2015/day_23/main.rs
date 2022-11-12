use std::collections::*;

#[derive(Clone, Debug)]
enum Instruction
{
    Hlf(char),
    Tpl(char),
    Inc(char),
    Jmp(i64),
    Jie(char, i64),
    Jio(char, i64),
}


fn parse(input: &[String]) -> Vec<Instruction>
{
    let to_char = |s: &str| s.chars().next().unwrap();
    let to_num = |s: &str| s.parse::<i64>().unwrap();

    input
        .iter()
        .map(|line| match line.split_whitespace().collect::<Vec<&str>>().as_slice()
        {
            ["hlf", r] => Instruction::Hlf(to_char(r)),
            ["tpl", r] => Instruction::Tpl(to_char(r)),
            ["inc", r] => Instruction::Inc(to_char(r)),
            ["jmp", offset] => Instruction::Jmp(to_num(offset)),
            ["jie", r, offset] => Instruction::Jie(to_char(r), to_num(offset)),
            ["jio", r, offset] => Instruction::Jio(to_char(r), to_num(offset)),

            _ => unreachable!(),
        })
        .collect()
}

fn solve(input: &[String], regs: HashMap<char, i64>) -> i64
{
    let mut regs = regs;
    let ins = parse(input);

    let mut ip = 0;
    use Instruction::*;
    while let Some(r#in) = ins.get(ip)
    {
        match &r#in
        {
            Hlf(r) => *regs.entry(*r).or_default() /= 2,
            Tpl(r) => *regs.entry(*r).or_default() *= 3,
            Inc(r) => *regs.entry(*r).or_default() += 1,
            Jmp(offset) => ip = (ip as i64 + offset) as usize,
            Jie(r, offset) =>
            {
                if *regs.entry(*r).or_default() % 2 == 0
                {
                    ip = (ip as i64 + offset) as usize;
                }
                else
                {
                    ip += 1;
                }
            },
            Jio(r, offset) =>
            {
                if *regs.entry(*r).or_default() == 1
                {
                    ip = (ip as i64 + offset) as usize;
                }
                else
                {
                    ip += 1;
                }
            },
        }

        if !matches!(r#in, Jmp(_)) && !matches!(r#in, Jie(_, _)) && !matches!(r#in, Jio(_, _))
        {
            ip += 1;
        }
    }

    regs[&'b']
}


fn task_one(input: &[String]) -> i64
{
    let regs = HashMap::new();
    solve(input, regs)
}
fn task_two(input: &[String]) -> i64
{
    let mut regs = HashMap::new();
    regs.insert('a', 1);
    solve(input, regs)
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
