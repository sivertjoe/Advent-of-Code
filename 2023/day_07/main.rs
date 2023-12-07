use std::collections::*;

trait Ordering {
    fn order(c1: &[u8], c2: &[u8]) -> std::cmp::Ordering;
}

struct Hand<O> {
    cards: Vec<u8>,
    id: usize,
    typ: HandType,
    _phant: std::marker::PhantomData<O>,
}

impl<O> PartialEq for Hand<O> {
    fn eq(&self, other: &Self) -> bool {
        self.cards == other.cards
    }
}
impl<O> Eq for Hand<O> {}

impl<O> Hand<O> {
    fn new(cards: Vec<u8>, id: usize) -> Self {
        Self {
            typ: HandType::new(&cards),
            id,
            cards,
            _phant: std::marker::PhantomData,
        }
    }
}

impl<O: Ordering> Ord for Hand<O> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let v1 = self.typ as u8;
        let v2 = other.typ as u8;

        v1.cmp(&v2)
            .then_with(|| O::order(&self.cards, &other.cards))
    }
}
impl<O: Ordering> PartialOrd for Hand<O> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, PartialOrd, PartialEq)]
enum HandType {
    FiveOfAKind = 6,
    FourOfAKind = 5,
    FullHouse = 4,
    ThreeOfAKind = 3,
    TwoPair = 2,
    OnePair = 1,
    HighCard = 0,
}

fn get_nums(cards: &[u8]) -> (u8, u8, u8) {
    // Datalayout
    // upper = char
    // lower = count
    let mut idx = 0;
    let mut arr: [u16; 5] = [0; 5];

    for card in cards {
        if let Some(entry) = arr.iter_mut().find(|entry| entry.to_be_bytes()[0] == *card) {
            let [upper, lower] = entry.to_be_bytes();
            *entry = u16::from_be_bytes([upper, lower + 1]);
        } else {
            arr[idx] = u16::from_be_bytes([*card, 1]);
            idx += 1;
        }
    }

    let mut smallest = std::u8::MAX;
    let mut second_smallest = std::u8::MAX;

    for entry in arr {
        let [ch, num] = entry.to_be_bytes();
        if ch == 0 {
            continue;
        }
        if num < smallest {
            second_smallest = smallest;
            smallest = num;
        } else if num < second_smallest {
            second_smallest = num;
        }
    }

    (idx as u8, smallest, second_smallest)
}

impl HandType {
    fn new(cards: &[u8]) -> Self {
        let (len, e1, e2) = get_nums(cards);

        match len {
            1 => Self::FiveOfAKind,
            2 => {
                if e1 == 1 {
                    Self::FourOfAKind
                } else {
                    Self::FullHouse
                }
            }
            3 => {
                if e1 == 1 && e2 == 1 {
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

fn parse<O>(input: &[String]) -> (HashMap<usize, usize>, Vec<Hand<O>>) {
    let mut vec = Vec::with_capacity(1000);
    let mut map = HashMap::with_capacity(1000);

    for (i, line) in input.iter().enumerate() {
        let (hand, bid) = line.split_once(' ').unwrap();
        let cards = hand.bytes().collect::<Vec<u8>>();
        assert!(cards.len() == 5);
        let bid = bid.parse::<usize>().unwrap();

        vec.push(Hand::new(cards, i));
        map.insert(i, bid);
    }
    (map, vec)
}

const fn task_one_compare() -> [u8; 128] {
    let mut vec = [0; 128];
    vec[b'2' as usize] = 0;
    vec[b'3' as usize] = 1;
    vec[b'4' as usize] = 2;
    vec[b'5' as usize] = 3;
    vec[b'6' as usize] = 4;
    vec[b'7' as usize] = 5;
    vec[b'8' as usize] = 6;
    vec[b'9' as usize] = 7;
    vec[b'T' as usize] = 8;
    vec[b'J' as usize] = 9;
    vec[b'Q' as usize] = 10;
    vec[b'K' as usize] = 11;
    vec[b'A' as usize] = 12;

    vec
}
struct Ord1;
impl Ordering for Ord1 {
    fn order(c1: &[u8], c2: &[u8]) -> std::cmp::Ordering {
        const ORD: [u8; 128] = task_one_compare();
        c1.iter()
            .map(|c| ORD[*c as usize])
            .cmp(c2.iter().map(|c| ORD[*c as usize]))
    }
}

fn _get_best_hand(vec: &mut [u8], idx: usize, best: &mut HandType) {
    if matches!(best, HandType::FiveOfAKind) {
        return;
    }
    for i in idx..vec.len() {
        let temp = vec[i];
        if temp == b'J' {
            for ch in [
                b'A', b'K', b'Q', b'T', b'9', b'8', b'7', b'6', b'5', b'4', b'3', b'2', b'J',
            ] {
                vec[i] = ch;

                let new = HandType::new(vec);
                if new > *best {
                    *best = new;
                }

                _get_best_hand(vec, idx + 1, best);
            }
            vec[i] = temp;
        }
    }
}

fn get_best_hand<O>(hand: &Hand<O>) -> HandType {
    let mut cards = hand.cards.clone();
    let mut best = hand.typ;
    _get_best_hand(&mut cards, 0, &mut best);
    best
}

const fn task_two_compare() -> [u8; 128] {
    let mut vec = [0; 128];
    vec[b'J' as usize] = 0;
    vec[b'2' as usize] = 1;
    vec[b'3' as usize] = 2;
    vec[b'4' as usize] = 3;
    vec[b'5' as usize] = 4;
    vec[b'6' as usize] = 5;
    vec[b'7' as usize] = 6;
    vec[b'8' as usize] = 7;
    vec[b'9' as usize] = 8;
    vec[b'T' as usize] = 9;
    vec[b'Q' as usize] = 10;
    vec[b'K' as usize] = 11;
    vec[b'A' as usize] = 12;

    vec
}

struct Ord2;
impl Ordering for Ord2 {
    fn order(c1: &[u8], c2: &[u8]) -> std::cmp::Ordering {
        const ORD: [u8; 128] = task_two_compare();
        c1.iter()
            .map(|c| ORD[*c as usize])
            .cmp(c2.iter().map(|c| ORD[*c as usize]))
    }
}

fn task_one(input: &[String]) -> usize {
    let (bids, mut hands) = parse::<Ord1>(input);
    hands.sort();
    hands
        .into_iter()
        .enumerate()
        .map(|(i, hand)| (i + 1) * bids[&hand.id])
        .sum()
}

fn task_two(input: &[String]) -> usize {
    let (bids, mut hands) = parse::<Ord2>(input);
    for hand in &mut hands {
        let typ = get_best_hand(hand);
        hand.typ = typ;
    }
    hands.sort();
    hands
        .into_iter()
        .enumerate()
        .map(|(i, hand)| (i + 1) * bids[&hand.id])
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
