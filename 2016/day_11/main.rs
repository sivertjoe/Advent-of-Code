use std::{
    collections::{hash_map::DefaultHasher, *},
    hash::{Hash, Hasher},
};

use Part::*;
use Type::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
#[repr(u8)]
enum Part
{
    Generator(Type),
    Microchip(Type),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
enum Type
{
    Th,
    St,
    Pl,
    Ru,
    Cu,
    El,
    Di,
}

#[derive(Clone)]
struct State
{
    floors:   Vec<BTreeSet<Part>>,
    elevator: usize,
}

impl Hash for State
{
    fn hash<H: Hasher>(&self, state: &mut H)
    {
        let generator_floor = |chip: Type| {
            for (i, floor) in self.floors.iter().enumerate()
            {
                for part in floor
                {
                    if matches!(part, Generator(chip2) if *chip2 == chip)
                    {
                        return i;
                    }
                }
            }
            unreachable!()
        };

        let mut vec = Vec::new();
        for (floor_number, floor) in self.floors.iter().enumerate()
        {
            for part in floor
            {
                if let Microchip(ch) = part
                {
                    let gen_floor = generator_floor(*ch);
                    vec.push((floor_number, gen_floor));
                }
            }
        }

        vec.sort();
        vec.hash(state);
        self.elevator.hash(state);
    }
}

impl State
{
    fn new(floors: Vec<BTreeSet<Part>>) -> State
    {
        State {
            floors,
            elevator: 0,
        }
    }

    fn is_valid(&self) -> bool
    {
        !self.floors[1..].iter().any(|floor| {
            floor.iter().any(|item| {
                if let &Microchip(e) = item
                {
                    !floor.iter().any(|t| t == &Generator(e))
                        && floor.iter().any(|t| matches!(t, Generator(ee) if *ee != e))
                }
                else
                {
                    false
                }
            })
        }) && self.elevator < self.floors.len()
    }

    fn next_states(&self) -> impl Iterator<Item = State> + '_
    {
        [false, true]
            .into_iter()
            .filter_map(|down| {
                (!(down && self.elevator == 0 || !down && self.elevator + 1 >= self.floors.len()))
                    .then(|| {
                        let new_elevator = if down { self.elevator - 1 } else { self.elevator + 1 };

                        let mut _parts: Vec<_> = self.floors[self.elevator].iter().collect();

                        let mut vec = Vec::with_capacity(_parts.len());
                        let mut i = 0;
                        while let Some(part1) = _parts.get(i)
                        {
                            i += 1;
                            for part2 in _parts[i..].iter().map(Some).chain(std::iter::once(None))
                            {
                                let mut new_floors = self.floors.clone();
                                new_floors[self.elevator].remove(part1);
                                new_floors[new_elevator].insert((*part1).clone());
                                if let Some(part2) = part2
                                {
                                    new_floors[self.elevator].remove(part2);
                                    new_floors[new_elevator].insert((*part2).clone());
                                }
                                let new_state = State {
                                    floors:   new_floors,
                                    elevator: new_elevator,
                                };
                                if new_state.is_valid()
                                {
                                    vec.push(new_state);
                                }
                            }
                        }
                        vec.into_iter()
                    })
            })
            .flatten()
    }
}

fn bfs(initial: State) -> usize
{
    let done = initial.floors.iter().map(|floor| floor.len()).sum();
    let mut states = VecDeque::new();
    states.push_back((initial, 1));
    let mut seen = HashSet::new();

    while let Some((state, depth)) = states.pop_front()
    {
        for new_state in state.next_states()
        {
            if new_state.floors[3].len() == done
            {
                return depth;
            }

            let mut hasher = DefaultHasher::new();
            new_state.hash(&mut hasher);
            let new_state_hash = hasher.finish();
            if !seen.contains(&new_state_hash)
            {
                seen.insert(new_state_hash);
                states.push_back((new_state, depth + 1));
            }
        }
    }
    unreachable!()
}

fn task_one(_input: &[String]) -> usize
{
    let state = State::new(vec![
        vec![Generator(St), Microchip(St), Generator(Pl), Microchip(Pl)]
            .into_iter()
            .collect(),
        vec![Generator(Th), Generator(Ru), Microchip(Ru), Generator(Cu), Microchip(Cu)]
            .into_iter()
            .collect(),
        vec![Microchip(Th)].into_iter().collect(),
        vec![].into_iter().collect(),
    ]);
    bfs(state)
}

fn task_two(_input: &[String]) -> usize
{
    let mut state = State::new(vec![
        vec![Generator(St), Microchip(St), Generator(Pl), Microchip(Pl)]
            .into_iter()
            .collect(),
        vec![Generator(Th), Generator(Ru), Microchip(Ru), Generator(Cu), Microchip(Cu)]
            .into_iter()
            .collect(),
        vec![Microchip(Th)].into_iter().collect(),
        vec![].into_iter().collect(),
    ]);
    state.floors[0].insert(Generator(El));
    state.floors[0].insert(Microchip(El));
    state.floors[0].insert(Generator(Di));
    state.floors[0].insert(Microchip(Di));
    bfs(state)
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
