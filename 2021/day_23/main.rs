use std::collections::HashMap;

use Amphipod::*;

fn read_input<P>(path: P, task: Task) -> State
where
    P: AsRef<std::path::Path>,
{
    let amphipod = |c| match c
    {
        b'A' => A,
        b'B' => B,
        b'C' => C,
        b'D' => D,
        _ => unreachable!(),
    };

    let get = |lns: &[&[u8]], x: usize, y: usize| {
        amphipod(match task
        {
            Task::Two => lns[x][y],
            Task::One => match x
            {
                3 => lns[5][y],
                x if x > 3 => match y
                {
                    3 => b'A',
                    5 => b'B',
                    7 => b'C',
                    9 => b'D',
                    _ => unreachable!(),
                },
                x if x < 3 => lns[x][y],
                _ => unreachable!(),
            },
        })
    };

    let s = std::fs::read_to_string(path).unwrap();

    let lns = s.lines().map(|l| l.as_bytes()).collect::<Vec<_>>();
    let hallway = [None; 7];

    let rooms = [
        vec![get(&lns, 5, 3), get(&lns, 4, 3), get(&lns, 3, 3), get(&lns, 2, 3)],
        vec![get(&lns, 5, 5), get(&lns, 4, 5), get(&lns, 3, 5), get(&lns, 2, 5)],
        vec![get(&lns, 5, 7), get(&lns, 4, 7), get(&lns, 3, 7), get(&lns, 2, 7)],
        vec![get(&lns, 5, 9), get(&lns, 4, 9), get(&lns, 3, 9), get(&lns, 2, 9)],
    ];

    State {
        hallway,
        rooms,
    }
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

fn main()
{
    let input = read_input(get_input_file(), Task::One);
    time(Task::One, task_one, input);
    let input = read_input(get_input_file(), Task::Two);
    time(Task::Two, task_two, input);
}

fn task_one(state: State) -> i32
{
    total_cost(state, &mut HashMap::new()) as i32
}

fn task_two(state: State) -> i32
{
    total_cost(state, &mut HashMap::new()) as i32
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Amphipod
{
    A,
    B,
    C,
    D,
}

#[inline]
fn cost(amphipod: &Amphipod) -> usize
{
    10_usize.pow(*amphipod as u32)
}

#[inline]
fn room_id(amphipod: &Amphipod) -> usize
{
    *amphipod as usize
}

type Room = Vec<Amphipod>;

#[derive(Clone, PartialEq, Eq, Hash)]
struct State
{
    hallway: [Option<Amphipod>; 7],
    rooms:   [Room; 4],
}

#[inline]
fn get_room<'a>(state: &'a State, room_type: &Amphipod) -> &'a Room
{
    &state.rooms[room_id(room_type)]
}

#[inline]
fn get_room_mut<'a>(state: &'a mut State, room_type: &Amphipod) -> &'a mut Room
{
    &mut state.rooms[room_id(room_type)]
}

#[inline]
fn room_ready(state: &State, room_type: &Amphipod) -> bool
{
    get_room(state, room_type).iter().all(|a| a == room_type)
}

fn steps_between(state: &State, hall_id: usize, room_type: &Amphipod) -> Option<usize>
{
    let room_id = room_id(room_type);

    let mut steps = 1;
    for step in (hall_id + 1..room_id + 2).chain(room_id + 2..hall_id)
    {
        if state.hallway[step].is_some()
        {
            return None;
        }
        steps += 2;
    }

    steps += 4 - get_room(state, room_type).len();
    steps += state.hallway[hall_id].is_none() as usize;
    steps -= matches!(hall_id, 0 | 6) as usize;
    Some(steps)
}

fn move_between(state: &State, hall_id: usize, room_type: &Amphipod) -> Option<(State, usize)>
{
    match state.hallway[hall_id]
    {
        Some(a) =>
        {
            if a != *room_type
                || get_room(state, room_type).len() == 4
                || !room_ready(state, room_type)
            {
                None
            }
            else
            {
                let steps_taken = steps_between(state, hall_id, room_type)?;
                let mut new_state = state.clone();
                new_state.hallway[hall_id] = None;
                get_room_mut(&mut new_state, room_type).push(*room_type);
                Some((new_state, steps_taken * cost(room_type)))
            }
        },

        _ =>
        {
            if get_room(state, room_type).is_empty() || room_ready(state, room_type)
            {
                None
            }
            else
            {
                let steps_taken = steps_between(state, hall_id, room_type)?;
                let mut new_state = state.clone();
                let move_amphipod = get_room_mut(&mut new_state, room_type).pop().unwrap();
                new_state.hallway[hall_id] = Some(move_amphipod);
                Some((new_state, steps_taken * cost(&move_amphipod)))
            }
        },
    }
}

fn total_cost(state: State, dp: &mut HashMap<State, usize>) -> usize
{
    if let Some(cost) = dp.get(&state)
    {
        return *cost;
    }

    if [A, B, C, D]
        .into_iter()
        .all(|room_type| get_room(&state, &room_type).as_slice() == [room_type; 4])
    {
        return 0;
    }


    let cost = (0..7)
        .map(|i| [A, B, C, D].into_iter().map(move |c| (i, c)))
        .flatten()
        .flat_map(|(hall_id, room_type)| move_between(&state, hall_id, &room_type))
        .map(|(new_state, move_cost)| move_cost.saturating_add(total_cost(new_state, dp)))
        .min()
        .unwrap_or(usize::MAX);
    dp.insert(state, cost);
    cost
}
