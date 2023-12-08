use itertools::Itertools;

#[derive(Eq, PartialEq, PartialOrd, Ord, Copy, Clone, Debug)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Copy, Clone, Debug)]
enum CardPartOne {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    T,
    J,
    Q,
    K,
    A,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Copy, Clone, Debug)]
enum CardPartTwo {
    J,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    T,
    Q,
    K,
    A,
}

trait FromChar {
    fn from_char(c: char) -> Self;
}

macro_rules! impl_from_char {
    ($ty:tt) => {
        impl FromChar for $ty {
            fn from_char(c: char) -> Self {
                match c {
                    '2' => Self::Two,
                    '3' => Self::Three,
                    '4' => Self::Four,
                    '5' => Self::Five,
                    '6' => Self::Six,
                    '7' => Self::Seven,
                    '8' => Self::Eight,
                    '9' => Self::Nine,
                    'T' => Self::T,
                    'J' => Self::J,
                    'Q' => Self::Q,
                    'K' => Self::K,
                    'A' => Self::A,
                    _ => unreachable!(),
                }
            }
        }
    };
}

impl_from_char!(CardPartOne);
impl_from_char!(CardPartTwo);

#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord)]
struct Hand<T> {
    cards: [T; 5],
    bid: usize,
}

impl<T: FromChar> Hand<T> {
    fn new(s: &str, bid: usize) -> Self {
        let arr = s.as_bytes();
        let cards = [
            T::from_char(arr[0] as char),
            T::from_char(arr[1] as char),
            T::from_char(arr[2] as char),
            T::from_char(arr[3] as char),
            T::from_char(arr[4] as char),
        ];
        Self { cards, bid }
    }
}

fn parse<T: FromChar>(input: &[String]) -> impl Iterator<Item = Hand<T>> + '_ {
    input.iter().map(|line| {
        let (hand, bid) = line.split_once(' ').unwrap();
        let bid = bid.parse::<usize>().unwrap();

        Hand::<T>::new(hand, bid)
    })
}

fn unique_and_dupes<T: Eq + Clone>(cards: &[T]) -> (Vec<T>, Vec<T>) {
    let mut unique = Vec::new();
    let mut seen = Vec::new();
    for card in cards {
        if !unique.contains(card) {
            unique.push(card.clone());
        } else {
            seen.push(card.clone());
        }
    }
    (unique, seen)
}

fn get_hand_strength<T: Clone + Eq>(hand: &Hand<T>) -> HandType {
    let sorted_cards = hand.cards.clone();
    let (dedup, duplicates) = unique_and_dupes(&sorted_cards);
    match (dedup.len(), duplicates.len()) {
        (5, 0) => HandType::HighCard,
        (1, 4) => HandType::FiveOfAKind,
        (4, 1) => HandType::OnePair,
        (2, 3) => {
            if duplicates[0] == duplicates[1] && duplicates[1] == duplicates[2] {
                HandType::FourOfAKind
            } else {
                HandType::FullHouse
            }
        }
        (3, 2) => {
            if duplicates[0] == duplicates[1] {
                HandType::ThreeOfAKind
            } else {
                HandType::TwoPair
            }
        }
        _ => unreachable!(),
    }
}

fn get_best_joker_hand(hand: &Hand<CardPartTwo>) -> HandType {
    let sorted_cards: Vec<_> = hand
        .cards
        .iter()
        .filter(|c| **c != CardPartTwo::J)
        .collect();

    let number_of_jokers = 5 - sorted_cards.len();
    let (dedup, duplicates) = unique_and_dupes(&sorted_cards);

    match (number_of_jokers, dedup.len(), duplicates.len()) {
        (5, _, _) => HandType::FiveOfAKind,
        (4, _, _) => HandType::FiveOfAKind,

        (3, 2, 0) => HandType::FourOfAKind,
        (3, 1, 1) => HandType::FiveOfAKind,

        (2, 3, 0) => HandType::ThreeOfAKind,
        (2, 2, 1) => HandType::FourOfAKind,
        (2, 1, 2) => HandType::FiveOfAKind,

        (1, 4, 0) => HandType::OnePair,
        (1, 3, 1) => HandType::ThreeOfAKind,
        (1, 1, 3) => HandType::FiveOfAKind,
        (1, 2, 2) => {
            if duplicates[0] == duplicates[1] {
                HandType::FourOfAKind
            } else {
                HandType::FullHouse
            }
        }

        // Default cases. No jokers
        _ => get_hand_strength(hand),
    }
}

fn solve<T, F>(input: &[String], f: F) -> usize
where
    T: FromChar + Ord,
    F: Fn(&Hand<T>) -> HandType,
{
    parse::<T>(input)
        .map(|hand| (f(&hand), hand))
        .sorted()
        .enumerate()
        .map(|(i, (_type, hand))| (i + 1) * hand.bid)
        .sum()
}

fn task_one(input: &[String]) -> usize {
    solve::<CardPartOne, _>(input, get_hand_strength)
}

fn task_two(input: &[String]) -> usize {
    solve::<CardPartTwo, _>(input, get_best_joker_hand)
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
