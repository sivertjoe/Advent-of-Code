#[derive(Debug, Clone)]
struct Boss
{
    hp:      i32,
    damage:  i32,
    effects: Vec<(usize, Spell)>,
}

#[derive(Debug, Clone)]
struct Player
{
    hp:        i32,
    armor:     i32,
    mana:      i32,
    mana_used: i32,
    effects:   Vec<(usize, Spell)>,
}

fn get_boss(input: &[String]) -> Boss
{
    let get = |n: usize| input[n].split_once(": ").unwrap().1.parse::<i32>().unwrap();
    Boss {
        hp: get(0), damage: get(1), effects: Vec::new()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Spell
{
    Missile,
    Drain,
    Shield,
    Poison,
    Recharge,
}

impl Spell
{
    fn mana_cost(&self) -> i32
    {
        use Spell::*;
        match self
        {
            Missile => 53,
            Drain => 73,
            Shield => 113,
            Poison => 173,
            Recharge => 229,
        }
    }
}
const SPELLS: [Spell; 5] =
    [Spell::Missile, Spell::Drain, Spell::Shield, Spell::Poison, Spell::Recharge];

fn apply_effects(player: &mut Player, boss: &mut Boss)
{
    player.effects.retain_mut(|(i, effect)| {
        *i -= 1;
        match effect
        {
            Spell::Shield =>
            {
                player.armor += 7;
                if *i == 0
                {
                    player.armor = 0;
                }
            },
            Spell::Recharge =>
            {
                player.mana += 101;
            },
            _ => unreachable!(),
        }
        *i > 0
    });
    boss.effects.retain_mut(|(i, effect)| {
        *i -= 1;
        match effect
        {
            Spell::Poison =>
            {
                boss.hp -= 3;
            },
            _ => unreachable!(),
        }
        *i > 0
    });
}

fn rec1(player: Player, boss: Boss, min: &mut i32, depth: i32, hard_mode: bool)
{
    let mut player = player;
    let mut boss = boss;


    // If you dont reach your answer by this level:
    // reach further
    if depth == 10
    {
        return;
    }

    if hard_mode
    {
        player.hp -= 1;
        if player.hp == 0
        {
            return;
        }
    }

    apply_effects(&mut player, &mut boss);

    if boss.hp <= 0
    {
        if player.mana_used < *min
        {
            *min = player.mana_used;
        }
        return;
    }

    let can_cast = SPELLS.iter().filter(|sp| {
        sp.mana_cost() <= player.mana
            && !player.effects.iter().any(|(_, s)| s == *sp)
            && !boss.effects.iter().any(|(_, s)| s == *sp)
    });


    for spell in can_cast
    {
        let mut player = player.clone();
        let mut boss = boss.clone();

        player.mana -= spell.mana_cost();
        player.mana_used += spell.mana_cost();
        match spell
        {
            Spell::Missile => boss.hp -= 4,
            Spell::Drain =>
            {
                boss.hp -= 2;
                player.hp += 2;
            },
            Spell::Shield =>
            {
                player.effects.push((6, Spell::Shield));
            },
            Spell::Poison =>
            {
                boss.effects.push((6, Spell::Poison));
            },
            Spell::Recharge =>
            {
                player.effects.push((5, Spell::Recharge));
            },
        }


        apply_effects(&mut player, &mut boss);
        if boss.hp <= 0
        {
            if player.mana_used < *min
            {
                *min = player.mana_used;
            }
            return;
        }

        let mut dmg = boss.damage - player.armor;
        if dmg < 1
        {
            dmg = 1;
        }

        player.hp -= dmg;
        if player.hp <= 0
        {
            return;
        }
        rec1(player, boss, min, depth + 1, hard_mode);
    }
}

fn task_one(input: &[String]) -> i32
{
    let boss = get_boss(input);
    let player =
        Player {
            hp: 50, armor: 0, mana: 500, effects: Vec::new(), mana_used: 0
        };
    let mut min = i32::MAX;
    rec1(player, boss, &mut min, 0, false);
    min
}

fn task_two(input: &[String]) -> i32
{
    let boss = get_boss(input);
    let player =
        Player {
            hp: 50, armor: 0, mana: 500, effects: Vec::new(), mana_used: 0
        };
    let mut min = i32::MAX;
    rec1(player, boss, &mut min, 0, true);
    min
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
