use std::collections::*;

#[derive(Clone)]
struct Entity
{
    hp:     i32,
    damage: i32,
    armor:  i32,
}

fn get_boss(input: &[String]) -> Entity
{
    let get = |n: usize| input[n].split_once(": ").unwrap().1.parse::<i32>().unwrap();
    Entity {
        hp: get(0), damage: get(1), armor: get(2)
    }
}

fn can_beat(player: Entity, boss: Entity) -> bool
{
    let mut player = player;
    let mut boss = boss;

    loop
    {
        if player.hp <= 0
        {
            return false;
        }

        boss.hp -= player.damage - boss.armor;
        if boss.hp <= 0
        {
            return true;
        }
        player.hp -= boss.damage - player.armor;
    }
}

#[derive(Clone, Copy)]
enum Stat
{
    Damage(i32),
    Defense(i32),
}

impl Stat
{
    fn unwrap(&self) -> i32
    {
        match self
        {
            Stat::Damage(d) => *d,
            Stat::Defense(d) => *d,
        }
    }
}

fn solve<Cmp>(input: &[String], initial: i32, cmp: Cmp, target: bool) -> i32
where
    Cmp: Fn(i32, i32) -> i32,
{
    use Stat::*;
    let boss = get_boss(input);
    let weapons =
        [(8, Damage(4)), (10, Damage(5)), (25, Damage(6)), (40, Damage(7)), (74, Damage(8))];
    let armors = [
        (0, Defense(0)),
        (13, Defense(1)),
        (31, Defense(2)),
        (53, Defense(3)),
        (75, Defense(4)),
        (102, Defense(5)),
    ];
    let rings = [
        (0, Damage(0)),
        (25, Damage(1)),
        (50, Damage(2)),
        (100, Damage(3)),
        (20, Defense(1)),
        (40, Defense(2)),
        (80, Defense(3)),
    ];

    let mut rrings = Vec::new();
    for x in 0..rings.len()
    {
        for y in 0..rings.len()
        {
            if x != 0 && x == y
            {
                continue;
            }
            rrings.push((rings[x], rings[y]));
        }
    }

    let mut val = initial;

    for (r1, r2) in rrings
    {
        for weapon in weapons.iter()
        {
            for armor in armors.iter()
            {
                let cost = weapon.0 + armor.0 + r1.0 + r2.0;

                let mut armor = armor.1.unwrap();
                let mut damage = weapon.1.unwrap();

                match r1.1
                {
                    Damage(d) => damage += d,
                    Defense(d) => armor += d,
                };
                match r2.1
                {
                    Damage(d) => damage += d,
                    Defense(d) => armor += d,
                };

                let player = Entity {
                    hp: 100,
                    armor,
                    damage,
                };

                if can_beat(player, boss.clone()) == target
                {
                    val = cmp(val, cost);
                }
            }
        }
    }

    val
}

fn task_one(input: &[String]) -> i32
{
    solve(input, i32::MAX, std::cmp::min, true)
}

fn task_two(input: &[String]) -> i32
{
    solve(input, 0, std::cmp::max, false)
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
