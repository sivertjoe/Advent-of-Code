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
    fn sum3(&self) -> usize {
        sum_n(4000) * 4
    }
}

const fn sum_n(n: usize) -> usize {
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
            None => Some((part.sum3(), self.res.clone())),
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

fn col(rules: &[Rule], workflows: &HashMap<String, Workflow>, curr: [(usize, usize); 4], ss: String) -> usize {
    let mut curr = curr;
    let mut sum = 0;

    const TH: usize = 4000 * 4000 * 4000;
    const FO: usize = TH * 4000;

    let _idx = |ch: char| {
        match ch {
            'x' => 0,
            'm' => 1,
            'a' => 2,
            's' => 3,
            _ => unreachable!()
        }
    };
    let idx = |r: &Rule| {
        match r.cmp {
            Some(Cmp::Less(ch, _)) => _idx(ch),
            Some(Cmp::Greater(ch, _)) => _idx(ch),
            _ => 0,
        }
    };

    let get_sum = |r: &Rule| {
        match r.cmp {
            None => 4000,
            Some(Cmp::Less(_, num)) => num,
            Some(Cmp::Greater(_, num)) => num
        }
    };

    let is_greater = |r: &Rule| 
        match r.cmp {
            Some(Cmp::Greater(_, _)) => true,
            _ => false
        };


    println!("before:{:?}", curr);
    for rule in rules {
        let mut new = curr.clone();
        let idx = idx(rule);
        match rule.cmp {
            None => {},
            Some(Cmp::Greater(_, num)) => {
                new[idx].0 = num+1;
                curr[idx].1 = num;
            }
            Some(Cmp::Less(_, num)) => {
                new[idx].1 = num-1;
                curr[idx].0 = num;
            }
        };
        println!("{ss} {:?}\nmy: {:?}\nother:{:?}\n\n", rule, curr, new);

        match &rule.res {
            Res::Accept => {
                println!("ACC {:?} {:?}", curr, new);
                sum += new.into_iter().map(|(st, ed)| (ed + 1).saturating_sub(st)).product::<usize>()
            }
            Res::Rule(s) => {
                let num = get_sum(&rule);
                match rule.cmp {
                    None => {
                        sum += ex(workflows, s.clone(), new);
                    },
                    Some(Cmp::Less(_, num)) => {
                        sum += ex(workflows, s.clone(), new);
                    }
                    Some(Cmp::Greater(_, num)) => {
                        sum += ex(workflows, s.clone(), new);
                    }
        };
            }
            Res::Reject => {
                let num = get_sum(&rule);
                //curr[idx(&rule)] = 4000 - num;
            }
        }
    }
    sum
}

fn _col(rules: &[Rule], workflows: &HashMap<String, Workflow>, curr: [(usize, usize); 4], ss: String) -> usize {
    let mut curr = curr;
    let mut sum = 0;

    const TH: usize = 4000 * 4000 * 4000;
    const FO: usize = TH * 4000;

    let _idx = |ch: char| {
        match ch {
            'x' => 0,
            'm' => 1,
            'a' => 2,
            's' => 3,
            _ => unreachable!()
        }
    };


    for rule in rules {
    }
    sum
}

// 167409079868000
// 167409079868000
// 35896635000000
fn ex(workflows: &HashMap<String, Workflow>, s: String, arr: [(usize, usize); 4]) -> usize {
    println!("EX {s}");
    let w = workflows.get(&s).unwrap();
    col(&w.0, &workflows, arr, s)
}

fn solve(workflows: &HashMap<String, Workflow>) -> usize {
    let mut initial = [(1, 4000), (1, 4000), (1,4000), (1,4000)];
    ex(&workflows, "in".to_string(), initial)
}

fn task_two(input: &[String]) -> usize {
    let (workflows, _parts) = parse(input);
    solve(&workflows)
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


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn only_accept() {
        let mut map = HashMap::new();
        map.insert("in".to_string(), Workflow(vec![Rule { cmp: None, res: Res::Accept } ]));
        assert_eq!(solve(&map), 4000*4000*4000*4000);
    }
    #[test]
    fn one_rule_to_accept_other_rule_accept_greater() {
        let mut map = HashMap::new();
        map.insert("in".to_string(), Workflow(vec![
            Rule { cmp: Some(Cmp::Greater('x', 500)), res: Res::Rule("qr".to_string()) },
            Rule { cmp: None, res: Res::Accept }

        ]));

        map.insert("qr".to_string(), Workflow(vec![
            Rule { cmp: None, res: Res::Accept }

        ]));
        assert_eq!(solve(&map), 4000*4000*4000*4000);
    }

    #[test]
    fn one_rule_to_accept_other_rule_accept_less() {
        let mut map = HashMap::new();
        map.insert("in".to_string(), Workflow(vec![
            Rule { cmp: Some(Cmp::Less('x', 500)), res: Res::Rule("qr".to_string()) },
            Rule { cmp: None, res: Res::Accept }

        ]));

        map.insert("qr".to_string(), Workflow(vec![
            Rule { cmp: None, res: Res::Accept }

        ]));
        assert_eq!(solve(&map), 4000*4000*4000*4000);
    }
    #[test]
    fn impossible_x_lessthan_500_and_greater_than_500() {
        let mut map = HashMap::new();
        map.insert("in".to_string(), Workflow(vec![
            Rule { cmp: Some(Cmp::Greater('x', 500)), res: Res::Rule("qr".to_string()) },
            Rule { cmp: None, res: Res::Reject }

        ]));
        map.insert("qr".to_string(), Workflow(vec![
            Rule { cmp: Some(Cmp::Less('x', 500)), res: Res::Accept },
            Rule { cmp: None, res: Res::Reject }

        ]));

        assert_eq!(solve(&map), 0);
    }
}
