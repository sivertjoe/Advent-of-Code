use std::collections::*;

#[derive(Debug, Clone)]
struct Hand {
    cards: Vec<char>,
}

#[derive(Debug)]
enum HandTypes {
    FiveOfAKind = 6,

    FourOfAKind = 5,
    FullHouse = 4,

    ThreeOfAKind = 3,
    TwoPair = 2,

    OnePair = 1,
    HighCard = 0,
}

impl HandTypes {
    fn new(hand: &Hand) -> Self {
        let mut map: HashMap<char, usize> = HashMap::new();
        for ch in hand.cards.iter().copied() {
            *map.entry(ch).or_default() += 1;
        }

        match map.len() {
            1 => Self::FiveOfAKind,
            2 => {
                let mut vec = map.values().collect::<Vec<_>>();
                vec.sort();
                if *vec[0] == 1 {
                    Self::FourOfAKind
                } else {
                    Self::FullHouse
                }
            }
            3 => {

                let mut vec = map.values().collect::<Vec<_>>();
                vec.sort();

                if *vec[0] == 1 && *vec[1] == 1 {
                    Self::ThreeOfAKind
                } else {
                    Self::TwoPair
                }
            }
            4 => Self::OnePair,

            5 => Self::HighCard,
            _ => unreachable!(),
        }
    }
}

fn compare((h1, _): &(Hand, usize), (h2, _): &(Hand, usize)) -> std::cmp::Ordering {
    let mut map: HashMap<char, usize> = HashMap::new();

    for (i, ch) in [
        'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2',
    ]
    .into_iter()
    .rev()
    .enumerate()
    {
        map.insert(ch, i);
    }

    let v1 = HandTypes::new(h1) as u8;
    let v2 = HandTypes::new(h2) as u8;

    if v1 > v2 {
        std::cmp::Ordering::Greater
    } else if v1 < v2 {
        std::cmp::Ordering::Less
    } else {
        for (ch1, ch2) in h1.cards.iter().zip(&h2.cards) {
            let e1 = map[ch1];
            let e2 = map[ch2];

            if e1 > e2 {
                return std::cmp::Ordering::Greater;
            } else if e1 < e2 {
                return std::cmp::Ordering::Less;
            }
        }
        unreachable!()
    }
}

fn parse(input: &[String]) -> Vec<(Hand, usize)> {
    input
        .iter()
        .map(|line| {
            let (hand, bid) = line.split_once(' ').unwrap();
            let cards = hand.chars().collect::<Vec<char>>();
            assert!(cards.len() == 5);
            let bid = bid.parse::<usize>().unwrap();
            (Hand { cards }, bid)
        })
        .collect()
}

fn task_one(input: &[String]) -> usize {
    let mut data = parse(input);

    data.sort_by(compare);
    data.into_iter()
        .enumerate()
        .map(|(i, (_hand, bid))| {
            bid * (i + 1)
        })
        .sum()
}

fn compare3(h1: &Hand, h2: &Hand) -> std::cmp::Ordering {
    let mut map: HashMap<char, usize> = HashMap::new();
    for (i, ch) in [
        'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J',
    ]
    .into_iter()
    .rev()
    .enumerate()
    {
        map.insert(ch, i);
    }

    for (ch1, ch2) in h1.cards.iter().zip(&h2.cards) {
        let e1 = map[ch1];
        let e2 = map[ch2];

        if e1 > e2 {
            return std::cmp::Ordering::Greater;
        } else if e1 < e2 {
            return std::cmp::Ordering::Less;
        }
    }
    std::cmp::Ordering::Equal
}

fn compare2((h1, _h1, _): &(Hand, Hand, usize), (h2, _h2, _): &(Hand, Hand, usize)) -> std::cmp::Ordering {
    let mut map: HashMap<char, usize> = HashMap::new();

    for (i, ch) in [
        'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J',
    ]
    .into_iter()
    .rev()
    .enumerate()
    {
        map.insert(ch, i);
    }

    let v1 = HandTypes::new(h1) as u8;
    let v2 = HandTypes::new(h2) as u8;

    if v1 > v2 {
        std::cmp::Ordering::Greater
    } else if v1 < v2 {
        std::cmp::Ordering::Less
    } else {
        compare3(_h1, _h2)
    }
}

fn _get_best_hand(vec: &Vec<char>, idx: usize, acc: &mut Vec<Hand>) {
    if idx >= vec.len() {
        return;
    }
    for i in idx..vec.len() {
        if vec[i] == 'J' {
            for  ch in [
                'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J',
            ] {
                let mut cc = vec.clone();
                cc[i] = ch;
                acc.push(Hand{ cards: cc.clone() });
                _get_best_hand(&cc, idx + 1, acc);
            }

        }
    }
}

fn get_best_hand(hand: Hand) -> Hand {
    let mut acc = Vec::new();
    acc.push(hand.clone());
    _get_best_hand(&hand.cards, 0, &mut acc);

    let mut acc: Vec<_> = acc.into_iter().map(|h| (h, hand.clone(), 0)).collect();

    acc.sort_by(compare2);
    acc.last().unwrap().0.clone()
}

fn task_two(input: &[String]) -> usize {
    let mut data: Vec<_> = parse(input).into_iter().map(|(hand, bid)| (get_best_hand(hand.clone()), hand, bid )).collect();

    data.sort_by(compare2);
    data.into_iter()
        .enumerate()
        .map(|(i, (_hand, __hand, bid))| {
            bid * (i + 1)
        })
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
