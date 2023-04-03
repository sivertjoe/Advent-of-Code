const AIR: u8 = b' ';

#[rustfmt::skip]
const H_LINE: [[u8; 4]; 4] =  [
    [b'#', b'#', b'#', b'#'],
    [b' ', b' ', b' ', b' '],
    [b' ', b' ', b' ', b' '],
    [b' ', b' ', b' ', b' ']
];

#[rustfmt::skip]
const CROSS: [[u8; 4]; 4] = [
    [b' ', b'#', b' ', b' '], 
    [b'#', b'#', b'#', b' '], 
    [b' ', b'#', b' ', b' '], 
    [b' ', b' ', b' ', b' ']
];

#[rustfmt::skip]
const L: [[u8; 4]; 4] = [
    [b'#', b'#', b'#', b' '], 
    [b' ', b' ', b'#', b' '], 
    [b' ', b' ', b'#', b' '], 
    [b' ', b' ', b' ', b' ']];

#[rustfmt::skip]
const V_LINE: [[u8; 4]; 4] =[
    [b'#', b' ', b' ', b' '], 
    [b'#', b' ', b' ', b' '], 
    [b'#', b' ', b' ', b' '], 
    [b'#', b' ', b' ', b' ']
];

#[rustfmt::skip]
const SQUARE: [[u8; 4]; 4] = [
    [b'#', b'#', b' ', b' '], 
    [b'#', b'#', b' ', b' '], 
    [b' ', b' ', b' ', b' '], 
    [b' ', b' ', b' ', b' ']
];

const SHAPES: [[[u8; 4]; 4]; 5] = [H_LINE, CROSS, L, V_LINE, SQUARE];

const MAP_HEIGHT: usize = 128;
const MAP_WIDTH: usize = 7;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Shape
{
    index: usize,
    pos:   (usize, usize),
}

struct Board
{
    contents:      [[u8; MAP_WIDTH]; MAP_HEIGHT],
    highest_shape: usize,
    map_height:    usize,
}

impl Board
{
    fn new() -> Self
    {
        Self {
            contents: [[AIR; MAP_WIDTH]; MAP_HEIGHT], highest_shape: 0, map_height: 20
        }
    }

    fn get(&self, pos: (usize, usize)) -> u8
    {
        self.contents[pos.1 % MAP_HEIGHT][pos.0]
    }

    fn set(&mut self, pos: (usize, usize), val: u8)
    {
        self.contents[pos.1 % MAP_HEIGHT][pos.0] = val;
    }
}

fn collide(board: &Board, shape: Shape) -> bool
{
    let pos = shape.pos;
    let shape = &SHAPES[shape.index];

    for (y, col) in shape.iter().enumerate()
    {
        for (x, item) in col.iter().enumerate()
        {
            let map_pos = (pos.0.wrapping_add(x), pos.1.wrapping_add(y));
            if map_pos.0 < MAP_WIDTH && map_pos.1 < board.map_height
            {
                if *item != AIR && board.get(map_pos) != AIR
                {
                    return true;
                }
            }
            else if shape[y][x] != AIR
            {
                return true;
            }
        }
    }
    false
}

fn add_shape(board: &mut Board, shape: Shape)
{
    let pos = shape.pos;
    let shape = &SHAPES[shape.index];
    for (y, col) in shape.iter().enumerate()
    {
        for (x, item) in col.iter().enumerate()
        {
            if *item != AIR
            {
                board.highest_shape = board.highest_shape.max(y + 1);
                board.set((pos.0 + x, pos.1 + y), shape[y][x]);
            }
        }
    }
    for y in board.highest_shape..board.highest_shape + 4
    {
        if (0..MAP_WIDTH).any(|x| board.get((x, y)) != AIR)
        {
            board.highest_shape = y + 1;
        }
    }
    for y in board.map_height..board.highest_shape + 20
    {
        for x in 0..MAP_WIDTH
        {
            board.set((x, y), AIR);
        }
    }
    board.map_height = board.highest_shape + 20;
}

enum Dir
{
    Left,
    Right,
}

fn parse(s: &str) -> Vec<Dir>
{
    s.bytes()
        .flat_map(|b| match b
        {
            b'<' => Some(Dir::Left),
            b'>' => Some(Dir::Right),
            _ => None,
        })
        .collect()
}

fn tetris<const N: usize>(s: &str) -> usize
{
    let dirs = parse(s);
    let mut dir_index = 0;

    let mut board = Board::new();

    let mut heights = Vec::new();
    let mut last_highest = 0;
    let mut last_shape = 1;
    let mut first_shape_delta = 0;
    let mut first_height_delta = 0;
    let mut shape_delta = 0;
    let mut height_diff = 0;
    let mut num_wraps = 0;

    for i_shape in 0..N
    {
        let mut shape = Shape {
            index: i_shape % SHAPES.len(),
            pos:   (2, board.highest_shape + 3),
        };
        let mut last_pos = shape.pos;

        while !collide(&board, shape)
        {
            last_pos = shape.pos;
            match dirs[dir_index]
            {
                Dir::Left => shape.pos.0 = shape.pos.0.wrapping_sub(1),
                Dir::Right => shape.pos.0 = shape.pos.0.wrapping_add(1),
            };
            dir_index = (dir_index + 1) % dirs.len();

            if dir_index == 0
            {
                num_wraps += 1;
                if num_wraps == 1
                {
                    first_height_delta = board.highest_shape - last_highest;
                    first_shape_delta = i_shape - last_shape;
                }
                height_diff = board.highest_shape - last_highest;
                shape_delta = i_shape - last_shape;
                last_highest = board.highest_shape;
                last_shape = i_shape;
            }

            if collide(&board, shape)
            {
                shape.pos = last_pos;
            }
            last_pos = shape.pos;
            shape.pos.1 = shape.pos.1.wrapping_sub(1);
        }

        shape.pos = last_pos;
        add_shape(&mut board, shape);
        heights.push(board.highest_shape);

        // Cycle detection
        if num_wraps >= 3 && i_shape >= last_shape + shape_delta - 1
        {
            let n2 = N;
            let last_shape_delta = (n2 - first_shape_delta - 1) % shape_delta;
            let n2_num_repeats = (n2 - first_shape_delta - last_shape_delta) / shape_delta;
            let n3 = last_shape + last_shape_delta;

            let last_height_delta = heights[n3] - last_highest;
            return (first_height_delta + n2_num_repeats * height_diff + last_height_delta) - 1;
        }
    }

    *heights.last().unwrap()
}
fn task_one(input: &[String]) -> usize
{
    tetris::<2022>(&input[0])
}

fn task_two(input: &[String]) -> usize
{
    tetris::<1_000_000_000_000>(&input[0]) + 1
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
