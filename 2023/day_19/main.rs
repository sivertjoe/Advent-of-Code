use std::collections::*;

#[derive(Debug)]
struct Part {
    x: usize,
    m: usize,
    a: usize,
    s: usize,
}

impl Part {
    fn get(&self, ch: char) -> usize {
        match ch {
            'x' => self.x,
            'm' => self.m,
            'a' => self.a,
            's' => self.s,
            _ => unreachable!(),
        }
    }
    fn sum(&self) -> usize {
        self.x + self.m + self.a + self.s
    }

    fn sum2(&self, ch: char) -> usize {
        let sum = sum_n(4000) * 3;
        match ch {
            'x' => sum + self.x,
            'm' => sum + self.m,
            'a' => sum + self.a,
            's' => sum + self.s,
            _ => unreachable!(),
        }
    }
    fn sum2(&self) -> usize {
        sum_n(4000) * 4
    }
}

fn sum_n(n: usize) -> usize {
    assert!((100 * (100 + 1)) / 2 == 5050);
    (n * (n + 1)) / 2
}

#[derive(Debug)]
enum Cmp {
    Less(char, usize),
    Greater(char, usize),
}

#[derive(Debug, Clone)]
enum Res {
    Rule(String),
    Accept,
    Reject,
}

#[derive(Debug)]
struct Rule {
    cmp: Option<Cmp>,
    res: Res,
}

impl Rule {
    fn apply(&self, part: &Part) -> Option<Res> {
        match self.cmp {
            None => Some(self.res.clone()),
            Some(Cmp::Less(ch, num)) if part.get(ch) < num => Some(self.res.clone()),
            Some(Cmp::Greater(ch, num)) if part.get(ch) > num => Some(self.res.clone()),
            _ => None,
        }
    }

    fn apply2(&self, part: &Part) -> Option<(usize, Res)> {
        /*match self.cmp {
            None => Some(self.res.clone()),
            Some(Cmp::Less(ch, num)) if part.get(ch) < num => Some(self.res.clone()),
            Some(Cmp::Greater(ch, num)) if part.get(ch) > num => Some(self.res.clone()),
            _ => None,
        }*/

        match self.cmp {
            None => ((
            Some(Cmp::Less(ch, num)) => {
                let rest = (4000 - num) - 1;
                Some((rest * part.sum2(ch), self.res.clone()))
            }
            Some(Cmp::Greater(ch, num)) => {
                let rest = 4000 - num;
                Some((rest * part.sum2(ch), self.res.clone()))
            }
        }
    }
}

#[derive(Debug)]
struct Workflow(Vec<Rule>);

impl Workflow {
    fn eval(&self, part: &Part) -> Res {
        self.0.iter().find_map(|rule| rule.apply(part)).unwrap()
    }
    fn eval2(&self, part: &Part) -> (usize, Res) {
        self.0.iter().find_map(|rule| rule.apply2(part)).unwrap()
    }
}

fn parse(input: &[String]) -> (HashMap<String, Workflow>, Vec<Part>) {
    let mut map = HashMap::new();
    let mut parts = Vec::new();

    let mut spl = input.split(|line| line.is_empty());

    for line in spl.next().unwrap() {
        let st = line.find('{').unwrap();
        let ed = line.len() - 1;
        let name = &line[..st];

        let mut workflow = Vec::new();
        let rule = &line[st + 1..ed];
        for rule in rule.split(',') {
            if rule.contains('>') {
                let mut iter = rule.split('>');
                let ch = iter.next().unwrap().chars().next().unwrap();

                let next = iter.next().unwrap();
                let (num, res) = next.split_once(':').unwrap();
                let num = num.parse::<usize>().unwrap();

                let res = match res {
                    "A" => Res::Accept,
                    "R" => Res::Reject,
                    next => Res::Rule(res.to_string()),
                };

                workflow.push(Rule {
                    cmp: Some(Cmp::Greater(ch, num)),
                    res,
                });
            } else if rule.contains('<') {
                let mut iter = rule.split('<');
                let ch = iter.next().unwrap().chars().next().unwrap();

                let next = iter.next().unwrap();
                let (num, res) = next.split_once(':').unwrap();
                let num = num.parse::<usize>().unwrap();

                let res = match res {
                    "A" => Res::Accept,
                    "R" => Res::Reject,
                    next => Res::Rule(res.to_string()),
                };

                workflow.push(Rule {
                    cmp: Some(Cmp::Less(ch, num)),
                    res,
                });
            } else {
                let res = match rule {
                    "A" => Res::Accept,
                    "R" => Res::Reject,
                    next => Res::Rule(next.to_string()),
                };
                workflow.push(Rule { cmp: None, res });
            }
        }
        map.insert(name.to_string(), Workflow(workflow));
    }

    let ch = |num: &str| {
        let (_, num) = num.split_once('=').unwrap();
        let num = num.parse::<usize>().unwrap();
        num
    };

    for line in spl.next().unwrap() {
        let line = &line[1..line.len() - 1];
        let mut iter = line.split(',');
        parts.push(Part {
            x: ch(iter.next().unwrap()),
            m: ch(iter.next().unwrap()),
            a: ch(iter.next().unwrap()),
            s: ch(iter.next().unwrap()),
        });
    }

    (map, parts)
}

fn evaluate(part: &Part, workflows: &HashMap<String, Workflow>) -> Option<usize> {
    let mut w = workflows.get("in").unwrap();
    loop {
        match w.eval(part) {
            Res::Accept => return Some(part.sum()),
            Res::Reject => return None,
            Res::Rule(new) => {
                w = workflows.get(&new).unwrap();
            }
        }
    }
}

fn task_one(input: &[String]) -> usize {
    let (workflows, parts) = parse(input);
    parts
        .into_iter()
        .flat_map(|part| evaluate(&part, &workflows))
        .sum()
}

fn evaluate2(part: &Part, workflows: &HashMap<String, Workflow>) -> Option<usize> {
    let mut w = workflows.get("in").unwrap();
    loop {
        match w.eval(part) {
            Res::Accept => return Some(part.sum()),
            Res::Reject => return None,
            Res::Rule(new) => {
                w = workflows.get(&new).unwrap();
            }
        }
    }
}

fn task_two(input: &[String]) -> usize {
    let (workflows, parts) = parse(input);
    parts
        .into_iter()
        .flat_map(|part| evaluate2(&part, &workflows))
        .sum()
}

fn main() {
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn read_input<P>(path: P) -> Vec<String>
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

enum Task {
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
    let elapsed = t.elapsed();
    let fmt = std::env::var("TASKUNIT").unwrap_or("ms".to_owned());

    let (u, elapsed) = match fmt.as_str() {
        "ms" => ("ms", elapsed.as_millis()),
        "ns" => ("ns", elapsed.as_nanos()),
        "us" => ("μs", elapsed.as_micros()),
        "s" => ("s", elapsed.as_secs() as u128),
        _ => panic!("unsupported time format"),
    };

    match task {
        Task::One => {
            println!("({}{u})\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        }
        Task::Two => {
            println!("({}{u})\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        }
    };
}

fn get_input_file() -> String {
    std::env::args()
        .nth(1)
        .unwrap_or_else(|| "input".to_string())
}
