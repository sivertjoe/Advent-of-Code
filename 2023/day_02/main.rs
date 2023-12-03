use itertools::Itertools;

enum Ball {
    Red,
    Blue,
    Green,
}

struct Game {
    id: usize,
    rounds: Vec<Vec<(Ball, usize)>>,
}

fn parse_round(s: &str) -> Vec<(Ball, usize)> {
    s.split(' ')
        .tuples()
        .map(|(num, color)| {
            let num = num.parse::<usize>().unwrap();
            let ball = match color {
                "red" => Ball::Red,
                "green" => Ball::Green,
                "blue" => Ball::Blue,
                _ => unreachable!(),
            };
            (ball, num)
        })
        .collect()
}

fn parse_game(line: &String) -> Game {
    let line = line.replace(',', "");
    let (id, rounds) = line.split_once(": ").unwrap();
    let id = id.split(' ').nth(1).unwrap().parse::<usize>().unwrap();

    let rounds = rounds.split("; ").map(parse_round).collect();

    Game { id, rounds }
}

fn task_one(input: &[String]) -> usize {
    const RED: usize = 12;
    const GREEN: usize = 13;
    const BLUE: usize = 14;
    input
        .iter()
        .map(parse_game)
        .filter_map(|game| {
            let is_valid = game.rounds.into_iter().all(|round| {
                round.into_iter().all(|(ball, num)| match ball {
                    Ball::Red => num <= RED,
                    Ball::Green => num <= GREEN,
                    Ball::Blue => num <= BLUE,
                })
            });
            is_valid.then_some(game.id)
        })
        .sum()
}

fn task_two(input: &[String]) -> usize {
    let get_fewest_cubes = |game: Game| -> (usize, usize, usize) {
        game.rounds
            .into_iter()
            .fold((0, 0, 0), |(mut r, mut g, mut b), round| {
                for (ball, num) in round {
                    match ball {
                        Ball::Red => r = r.max(num),
                        Ball::Green => g = g.max(num),
                        Ball::Blue => b = b.max(num),
                    }
                }
                (r, g, b)
            })
    };

    input
        .iter()
        .map(|line| {
            let game = parse_game(line);
            let (r, g, b) = get_fewest_cubes(game);
            r * g * b
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
