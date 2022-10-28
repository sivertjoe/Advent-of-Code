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
    Snd(char),
    Rcv(char),
    Set(char, Value),
    Mul(char, Value),
    Add(char, Value),
    Mod(char, Value),
    Jgz(Value, Value),
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
            ["snd", c] => Instruction::Snd(to_char(c)),
            ["rcv", c] => Instruction::Rcv(to_char(c)),
            ["jgz", x, y] => Instruction::Jgz(to_val(x), to_val(y)),
            ["set", x, y] => Instruction::Set(to_char(x), to_val(y)),
            ["add", x, y] => Instruction::Add(to_char(x), to_val(y)),
            ["mul", x, y] => Instruction::Mul(to_char(x), to_val(y)),
            ["mod", x, y] => Instruction::Mod(to_char(x), to_val(y)),

            _ => unimplemented!(),
        })
        .collect()
}

#[derive(Debug)]
struct Computer
{
    instructions:    Vec<Instruction>,
    program_counter: usize,
    registers:       HashMap<char, i64>,
    input:           VecDeque<i64>,
    send_count:      usize,
}

impl Computer
{
    fn new(instructions: Vec<Instruction>, id: i64) -> Self
    {
        Self {
            instructions,
            program_counter: 0,
            registers: std::iter::once(('p', id)).collect(),
            input: VecDeque::new(),
            send_count: 0,
        }
    }

    fn execute_instruction(&mut self, other: &mut VecDeque<i64>) -> bool
    {
        if self.program_counter >= self.instructions.len()
        {
            return true;
        }

        let registers = &mut self.registers;
        use Instruction::*;
        match &self.instructions[self.program_counter]
        {
            Add(x, y) =>
            {
                *registers.entry(*x).or_insert(0) += y.get(&registers);
                self.program_counter += 1;
            },
            Mul(x, y) =>
            {
                *registers.entry(*x).or_insert(0) *= y.get(&registers);
                self.program_counter += 1;
            },
            Mod(x, y) =>
            {
                let val = y.get(&registers);
                let reg = registers.entry(*x).or_insert(0);
                *reg = *reg % val;
                self.program_counter += 1;
            },

            Set(x, y) =>
            {
                *registers.entry(*x).or_insert(0) = y.get(&registers);
                self.program_counter += 1;
            },

            Snd(x) =>
            {
                let val = *registers.entry(*x).or_insert(0);
                other.push_back(val);
                self.send_count += 1;
                self.program_counter += 1;
            },
            Rcv(x) =>
            {
                if self.input.is_empty()
                {
                    return true;
                }
                registers.insert(*x, self.input.pop_front().unwrap());
                self.program_counter += 1;
            },
            Jgz(x, y) =>
            {
                let val = y.get(&registers);
                let val2 = x.get(&registers);

                if val2 > 0
                {
                    self.program_counter = (self.program_counter as i64 + val) as usize;
                }
                else
                {
                    self.program_counter += 1;
                }
            },
        }

        false
    }
}

fn task_one(input: &[String]) -> i64
{
    use Instruction::*;
    let ins = parse(input);
    let mut registers = HashMap::new();

    let mut prev = 0;
    let mut ip = 0;

    loop
    {
        match &ins[ip]
        {
            Add(x, y) =>
            {
                *registers.entry(*x).or_insert(0) += y.get(&registers);
                ip += 1;
            },
            Mul(x, y) =>
            {
                *registers.entry(*x).or_insert(0) *= y.get(&registers);
                ip += 1;
            },
            Mod(x, y) =>
            {
                let val = y.get(&registers);
                let reg = registers.entry(*x).or_insert(0);
                *reg = *reg % val;
                ip += 1;
            },

            Set(x, y) =>
            {
                *registers.entry(*x).or_insert(0) = y.get(&registers);
                ip += 1;
            },

            Snd(x) =>
            {
                prev = *registers.entry(*x).or_insert(0);
                ip += 1;
            },
            Rcv(x) =>
            {
                let v = *registers.entry(*x).or_insert(0);
                if v > 0
                {
                    return prev;
                }
                ip += 1;
            },
            Jgz(x, y) =>
            {
                let val = y.get(&registers);
                let val2 = x.get(&registers);
                if val2 > 0
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
}

fn task_two(input: &[String]) -> usize
{
    let ins = parse(input);
    let mut c0 = Computer::new(ins.clone(), 0);
    let mut c1 = Computer::new(ins.clone(), 1);

    loop
    {
        let r0 = c0.execute_instruction(&mut c1.input);
        let r1 = c1.execute_instruction(&mut c0.input);
        if r0 && r1
        {
            break;
        }
    }

    c1.send_count
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
