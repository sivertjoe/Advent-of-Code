use std::collections::*;

#[derive(Debug)]
enum Type
{
    Open,
    Number(u32),
}

type Board = HashMap<(usize, usize), Type>;

fn get_board(input: &[String]) -> Board
{
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate()
    {
        for (x, ch) in line.chars().enumerate()
        {
            let typ = match ch
            {
                '#' => continue,
                '.' => Type::Open,
                num => Type::Number(num.to_digit(10).unwrap()),
            };
            map.insert((x, y), typ);
        }
    }
    map
}

fn calculate_distance_vectors(board: &Board) -> Vec<Vec<usize>>
{
    let mut dist = board
        .values()
        .filter_map(|v| match v
        {
            Type::Number(num) => Some(*num),
            _ => None,
        })
        .collect::<Vec<_>>();

    dist.sort();

    dist.into_iter()
        .map(|num| {
            let mut vec = get_distance_vector(num, board);
            vec.sort();
            vec.into_iter().map(|v| v.1).collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn solve(input: &[String], end: Option<usize>) -> usize
{
    let board = get_board(input);
    let dist = calculate_distance_vectors(&board);


    // We can skip 0 since we start at it
    let perms = (1..dist.len()).collect::<Vec<_>>();
    let perms = unique_permutations(perms);


    let mut shortest = usize::MAX;
    for perm in perms
    {
        let mut cost = 0;
        let mut pos = 0;
        for i in perm.into_iter().chain(end.into_iter())
        {
            cost += dist[pos][i];
            pos = i;
        }
        shortest = std::cmp::min(shortest, cost);
    }
    shortest
}

fn unique_permutations<T: Clone>(items: Vec<T>) -> Vec<Vec<T>>
where
    T: Ord,
{
    if items.len() == 1
    {
        vec![items]
    }
    else
    {
        let mut output: Vec<Vec<T>> = vec![];

        let mut unique_items = items.clone();
        unique_items.sort();
        unique_items.dedup();
        for first in unique_items
        {
            let mut remaining_elements = items.clone();

            let index = remaining_elements.iter().position(|x| *x == first).unwrap();
            remaining_elements.remove(index);

            for mut permutation in unique_permutations(remaining_elements)
            {
                permutation.insert(0, first.clone());
                output.push(permutation);
            }
        }
        output
    }
}

fn ns((x, y): (usize, usize), board: &Board) -> impl Iterator<Item = (usize, usize)> + '_
{
    let x = x as isize;
    let y = y as isize;

    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        .into_iter()
        .filter(|(x, y)| *x >= 0 && *y >= 0 && board.contains_key(&(*x as usize, *y as usize)))
        .map(|(x, y)| (x as usize, y as usize))
}

fn get_distance_vector(num: u32, board: &Board) -> Vec<(u32, usize)>
{
    let mut res = Vec::new();
    let mut vec = VecDeque::new();
    let start = *board
        .iter()
        .find(|(_pos, t)| matches!(t, Type::Number(n) if *n == num))
        .map(|(pos, _t)| pos)
        .unwrap();
    let mut seen = HashSet::new();
    seen.insert(start);

    vec.push_back((start, 0));
    while let Some((pos, cost)) = vec.pop_front()
    {
        if let Some(Type::Number(num)) = board.get(&pos)
        {
            res.push((*num, cost));
        }

        for neighbor in ns(pos, board)
        {
            if seen.insert(neighbor)
            {
                vec.push_back((neighbor, cost + 1));
            }
        }
    }
    res
}

fn task_one(input: &[String]) -> usize
{
    solve(input, None)
}

fn task_two(input: &[String]) -> usize
{
    solve(input, Some(0))
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
