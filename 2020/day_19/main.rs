enum Rule
{
    Base(u8),
    Single(Vec<usize>),
    Double(Vec<usize>, Vec<usize>),
}

fn read_input() -> (Vec<Rule>, Vec<String>)
{
    let content = std::fs::read_to_string("input").unwrap();
    let (rules, msgs) = content.split_once("\n\n").unwrap();

    let mut rules: Vec<(usize, Rule)> = rules
        .lines()
        .map(|line| {
            let (n, rule) = line.split_once(": ").unwrap();
            (
                n.parse().unwrap(),
                if rule.starts_with('"')
                {
                    Rule::Base(rule.chars().nth(1).unwrap() as u8)
                }
                else
                {
                    let parts: Vec<_> = rule.split('|').collect();
                    let a = parts[0]
                        .split_whitespace()
                        .flat_map(<usize as std::str::FromStr>::from_str)
                        .collect();

                    if parts.len() == 1
                    {
                        Rule::Single(a)
                    }
                    else
                    {
                        let b = parts[1]
                            .split_whitespace()
                            .flat_map(<usize as std::str::FromStr>::from_str)
                            .collect();
                        Rule::Double(a, b)
                    }
                },
            )
        })
        .collect();

    rules.sort_unstable_by_key(|r| r.0);
    let rules = rules.into_iter().map(|r| r.1).collect();
    let msgs = msgs.lines().map(String::from).collect();

    (rules, msgs)
}

fn solve<F>(msgs: &[String], f: F) -> usize
where
    F: Fn(&&String) -> bool,
{
    msgs.into_iter().filter(f).count()
}

fn part_one(rules: &[Rule], msgs: &[String]) -> usize
{
    let f =
        |msg: &&String| matches(msg.as_bytes(), 0, &rules).map(|n| n.len() == 0).unwrap_or(false);
    solve(msgs, f)
}

fn part_two(rules: Vec<Rule>, msgs: Vec<String>) -> usize
{
    let mut rules = rules;
    /*
     * 8 : 42 | 42 8
     * 11: 42 31 | 42 11 31
     */
    rules[8] = Rule::Double(vec![42], vec![42, 8]);
    rules[11] = Rule::Double(vec![42, 31], vec![42, 11, 31]);

    let f = |msg: &&String| matches_42(msg.as_bytes(), &rules, 0);
    solve(&msgs, f)
}

pub fn main()
{
    let (rules, msgs) = read_input();

    println!("{}", part_one(&rules, &msgs));
    println!("{}", part_two(rules, msgs));
}

fn matches_42(msg: &[u8], rules: &[Rule], depth: usize) -> bool
{
    match matches(msg, 42, rules)
    {
        Some(msg) if matches_31(0, depth, msg, rules) => true,
        Some(msg) => matches_42(msg, rules, depth + 1),
        None => false,
    }
}

fn matches_31(depth: usize, max_depth: usize, msg: &[u8], rules: &[Rule]) -> bool
{
    if depth == max_depth
    {
        false
    }
    else
    {
        match matches(msg, 31, rules)
        {
            Some(msg) if msg.is_empty() => true,
            Some(msg) => matches_31(depth + 1, max_depth, msg, rules),
            None => false,
        }
    }
}

fn matches<'m>(msg: &'m [u8], rule: usize, rules: &[Rule]) -> Option<&'m [u8]>
{
    if msg.is_empty()
    {
        return None;
    }

    match &rules[rule]
    {
        Rule::Base(c) => (&msg[0] == c).then_some(&msg[1..]),
        Rule::Single(a) => a.into_iter().try_fold(msg, |m, r| matches(m, *r, rules)),
        Rule::Double(a, b) => a
            .into_iter()
            .try_fold(msg, |m, &r| matches(m, r, rules))
            .or_else(|| b.into_iter().try_fold(msg, |m, r| matches(m, *r, rules))),
    }
}
