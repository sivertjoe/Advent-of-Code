struct Bingo
{
    numbers: Vec<i32>,
    boards:  Vec<Vec<(i32, bool)>>,
}

fn read_input<P>(path: P) -> Bingo
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    let mut reader =
        std::io::BufReader::new(file).lines().flatten().filter(|line| !line.is_empty());

    let numbers: Vec<_> =
        reader.next().unwrap().split(",").map(|n| n.parse::<i32>().unwrap()).collect();

    let mut vec: Vec<_> = reader
        .map(|line| line.split(" ").flat_map(|n| n.parse::<i32>()).collect::<Vec<_>>())
        .collect();

    let mut res = Vec::new();
    while !vec.is_empty()
    {
        let board: Vec<(i32, bool)> = vec.drain(0..5).flatten().map(|n| (n, false)).collect();
        res.push(board);
    }

    Bingo {
        numbers,
        boards: res,
    }
}

fn time<F, T, U>(pre: &'static str, f: F, arg: T)
where
    F: Fn(T) -> U,
    U: std::fmt::Display,
{
    let t0 = std::time::Instant::now();
    let res = f(arg);
    let t1 = std::time::Instant::now();
    println!("Task {}: {}\t({}ms)", pre, res, t1.duration_since(t0).as_millis());
}

fn main()
{
    let bingo = read_input("input");
    time("one", task_one, bingo);
    let bingo = read_input("input");
    time("two", task_two, bingo);
}

fn task_one(mut bingo: Bingo) -> i32
{
    for number in bingo.numbers
    {
        for board in bingo.boards.iter_mut()
        {
            if let Some(pos) = board.iter().position(|(n, _)| *n == number)
            {
                board[pos].1 = true;

                if check_bingo(&board, pos)
                {
                    let sum = board.iter().filter_map(|(n, b)| (!b).then(|| *n)).sum::<i32>();
                    return sum * number;
                }
            }
        }
    }

    unreachable!()
}

fn task_two(bingo: Bingo) -> i32
{
    let (numbers, mut boards) = (bingo.numbers, bingo.boards);

    for number in numbers
    {
        let len = boards.len();
        let mut new_boards = Vec::with_capacity(len);

        for mut board in boards.into_iter()
        {
            let pos = match board.iter().position(|(n, _)| *n == number)
            {
                None =>
                {
                    new_boards.push(board);
                    continue;
                },
                Some(idx) => idx,
            };

            board[pos].1 = true;

            if !check_bingo(&board, pos)
            {
                new_boards.push(board);
            }
            else if len == 1
            {
                let sum = board.iter().filter_map(|(n, b)| (!b).then(|| *n)).sum::<i32>();
                return sum * number;
            }
        }
        boards = new_boards;
    }

    unreachable!()
}

#[inline]
fn check_bingo(board: &[(i32, bool)], idx: usize) -> bool
{
    // check horizontal
    let hor = (idx / 5) * 5;

    // check vertical
    let vert = idx % 5;

    (hor..hor + 5).all(|i| board[i].1) || (0..5).all(|i| board[vert + (i * 5)].1)
}
