struct Room
{
    name:       String,
    section_id: usize,
    checksum:   String,
}

fn parse_room(line: &String) -> Room
{
    let (name, first) = line.rsplit_once('-').unwrap();

    let name = name.to_string();

    let (section_id, checksum) = first.split_once('[').unwrap();
    let section_id = section_id.parse::<usize>().unwrap();
    let checksum = checksum[..checksum.len() - 1].to_owned();

    Room {
        name,
        section_id,
        checksum,
    }
}

#[inline(always)]
fn letter(letters: &mut [u8; 26]) -> u8
{
    let mut max = 0;
    let mut index = 0;

    for (i, ch) in letters.iter().enumerate()
    {
        if *ch > max
        {
            max = *ch;
            index = i;
        }
    }
    letters[index] = 0;
    index as u8 + b'a'
}

#[inline(always)]
fn matching_checksum(room: &Room) -> bool
{
    let mut arr = [0; 26];

    for ch in room.name.chars()
    {
        if ch != '-'
        {
            arr[(ch as u8 - b'a') as usize] += 1;
        }
    }

    letter(&mut arr) == room.checksum.as_bytes()[0]
        && letter(&mut arr) == room.checksum.as_bytes()[1]
        && letter(&mut arr) == room.checksum.as_bytes()[2]
        && letter(&mut arr) == room.checksum.as_bytes()[3]
        && letter(&mut arr) == room.checksum.as_bytes()[4]
}

#[inline(always)]
fn parse(input: &[String]) -> Vec<Room>
{
    input.iter().map(parse_room).collect()
}

#[inline(always)]
fn real_rooms(input: &[String]) -> impl Iterator<Item = Room>
{
    parse(input).into_iter().filter(|room| matching_checksum(&room))
}

fn task_one(input: &[String]) -> usize
{
    real_rooms(input).map(|room| room.section_id).sum()
}

#[inline(always)]
fn storage_object_northpole(room: &Room) -> bool
{
    let shift = room.section_id;
    let s = "northpole-object-storage".as_bytes();

    for (i, ch) in room.name.chars().enumerate()
    {
        if ch != '-'
        {
            let letter = ((((ch as u8 - b'a') as usize + shift) % 26) as u8 + b'a') as char;
            if s[i] as char != letter
            {
                return false;
            }
        }
    }
    true
}

#[inline(always)]
fn task_two(input: &[String]) -> usize
{
    real_rooms(input)
        .find(|room| storage_object_northpole(&room))
        .unwrap()
        .section_id
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
