use std::collections::*;

#[derive(Clone, Copy, Debug)]
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
    Cpy(Value, char),
    Inc(char),
    Dec(char),
    Jnz(Value, Value),
    Tgl(char),
    Out(Value),
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
            ["cpy", x, y] => Instruction::Cpy(to_val(x), to_char(y)),
            ["inc", x] => Instruction::Inc(to_char(x)),
            ["dec", x] => Instruction::Dec(to_char(x)),
            ["tgl", x] => Instruction::Tgl(to_char(x)),
            ["out", x] => Instruction::Out(to_val(x)),

            _ => unreachable!(),
        })
        .collect()
}

fn solve(input: &[String], regs: HashMap<char, i64>) -> bool
{
    let mut out = Vec::new();
    let mut regs = regs;
    let mut ins = parse(input);

    let mut ip = 0;
    use Instruction::*;
    while (0..ins.len()).contains(&ip)
    {
        let r#in = ins[ip].clone();

        match &r#in
        {
            Cpy(a, b) => *regs.entry(*b).or_insert(0) = a.get(&regs),
            Inc(a) => *regs.entry(*a).or_insert(0) += 1,
            Dec(a) => *regs.entry(*a).or_insert(0) -= 1,
            Jnz(a, b) =>
            {
                let a = a.get(&regs);
                let b = b.get(&regs);
                if a != 0
                {
                    ip = (ip as i64 + b) as usize;
                }
                else
                {
                    ip += 1;
                }
            },
            Tgl(a) =>
            {
                let a = *regs.entry(*a).or_insert(0);
                let nip = (ip as i64 + a) as usize;
                let Some(instruction) = ins.get_mut(nip) else { ip += 1; continue; };
                match instruction
                {
                    Inc(a) => *instruction = Dec(*a),
                    Tgl(a) => *instruction = Inc(*a),
                    Dec(a) => *instruction = Inc(*a),

                    Jnz(x, y) =>
                    {
                        let Value::Reg(ch) = y else { panic!("must be register"); };
                        *instruction = Cpy(*x, *ch);
                    },
                    Cpy(x, y) => *instruction = Jnz(*x, Value::Reg(*y)),
                    _ =>
                    {},
                }
            },
            Out(a) =>
            {
                let v = a.get(&regs);
                out.push(v);
            },
        }

        if out.len() == 10
        {
            if out == vec![0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
            {
                return true;
            }
            return false;
        }

        if !matches!(r#in, Jnz(_, _))
        {
            ip += 1;
        }
    }
    false
}


fn task_one(input: &[String]) -> usize
{
    for i in 0..
    {
        let mut regs = HashMap::new();
        regs.insert('a', i as i64);
        if solve(input, regs)
        {
            return i;
        }
    }
    unreachable!()
}

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
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
