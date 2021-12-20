use std::collections::*;
type Scanner = Vec<Point>;
type Point = (i32, i32, i32);

fn read_input<P>(path: P) -> Vec<Scanner>
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();

    let mut vec = Vec::new();
    for line in std::io::BufReader::new(file).lines().flatten().filter(|line| !line.is_empty())
    {
        if line.starts_with("---")
        {
            vec.push(Vec::new());
            continue;
        }

        let arr: Vec<i32> = line.split(',').flat_map(|l| l.parse::<i32>()).collect();
        vec.last_mut().unwrap().push((arr[0], arr[1], arr[2]));
    }
    vec
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
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn task_one(input: &[Scanner]) -> i32
{
    let (len, _) = solve(input);
    len
}

fn task_two(input: &[Scanner]) -> i32
{
    let (_, beacons) = solve(input);
    let mut max = 0;
    for (i, beacon) in beacons.iter().enumerate()
    {
        for beacon2 in beacons.iter().skip(i + 1)
        {
            max = std::cmp::max(manhatten_distance(beacon, beacon2), max);
        }
    }
    max
}

fn add_points(a: &Point, b: &Point) -> Point
{
    (a.0 + b.0, a.1 + b.1, a.2 + b.2)
}

fn sub_points(a: &Point, b: &Point) -> Point
{
    (a.0 - b.0, a.1 - b.1, a.2 - b.2)
}

fn manhatten_distance(p1: &Point, p2: &Point) -> i32
{
    (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs() + (p1.2 - p2.2).abs()
}

fn solve(input: &[Scanner]) -> (i32, Vec<Point>)
{
    // Sheeeeeeesh
    let rotations: Vec<Box<dyn Fn(Point) -> Point>> = vec![
        Box::new(|(x, y, z)| (x, y, z)),
        Box::new(|(x, y, z)| (x, -y, -z)),
        Box::new(|(x, y, z)| (x, z, -y)),
        Box::new(|(x, y, z)| (x, -z, y)),
        Box::new(|(x, y, z)| (y, x, -z)),
        Box::new(|(x, y, z)| (y, -x, z)),
        Box::new(|(x, y, z)| (y, z, x)),
        Box::new(|(x, y, z)| (y, -z, -x)),
        Box::new(|(x, y, z)| (z, x, y)),
        Box::new(|(x, y, z)| (z, -x, -y)),
        Box::new(|(x, y, z)| (z, y, -x)),
        Box::new(|(x, y, z)| (z, -y, x)),
        Box::new(|(x, y, z)| (-x, y, -z)),
        Box::new(|(x, y, z)| (-x, -y, z)),
        Box::new(|(x, y, z)| (-x, z, y)),
        Box::new(|(x, y, z)| (-x, -z, -y)),
        Box::new(|(x, y, z)| (-y, x, z)),
        Box::new(|(x, y, z)| (-y, -x, -z)),
        Box::new(|(x, y, z)| (-y, z, -x)),
        Box::new(|(x, y, z)| (-y, -z, x)),
        Box::new(|(x, y, z)| (-z, x, -y)),
        Box::new(|(x, y, z)| (-z, -x, y)),
        Box::new(|(x, y, z)| (-z, y, x)),
        Box::new(|(x, y, z)| (-z, -y, -x)),
    ];


    let mut matched = HashSet::new();
    matched.insert(0);

    let mut curr_scanner: HashSet<Point> = input[0].clone().into_iter().collect();

    // scanner 0 is the first beacon
    let mut beacons = vec![(0, 0, 0)];
    while matched.len() < input.len()
    {
        for rotate in &rotations
        {
            let mut in_common = HashMap::new();
            for point1 in curr_scanner.clone()
            {
                for (id, scanner) in input.iter().enumerate()
                {
                    if !matched.contains(&id)
                    {
                        for point2 in scanner
                        {
                            let diff = sub_points(&point1, &rotate(*point2));
                            let entry = in_common.entry(diff).or_insert(0);
                            *entry += 1;

                            if *entry >= 12
                            {
                                matched.insert(id);
                                beacons.push(diff);
                                curr_scanner
                                    .extend(scanner.iter().map(|k| add_points(&rotate(*k), &diff)));
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    (curr_scanner.len() as i32, beacons)
}
