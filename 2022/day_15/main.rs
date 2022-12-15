type Point = (isize, isize);

fn manhattan(p1: Point, p2: Point) -> isize
{
    (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs()
}

fn parse(line: &str) -> (Point, Point)
{
    let spl = line
        .split([' ', '=', ':', ','])
        .flat_map(|tok| tok.parse::<isize>())
        .collect::<Vec<_>>();

    let x = spl[0];
    let y = spl[1];
    let z = spl[2];
    let w = spl[3];

    ((x, y), (z, w))
}
fn task_one(input: &[String]) -> isize
{
    let input: Vec<_> = input.iter().map(|line| parse(line)).collect();
    let dists = input.iter().map(|(p1, p2)| manhattan(*p1, *p2)).collect::<Vec<_>>();

    let y = 2_000_000;

    let mut intervals = input
        .iter()
        .map(|(s, _b)| s)
        .zip(dists.iter())
        .filter_map(|(s, dist)| {
            let dx = dist - (s.1 - y).abs();
            (dx >= 0).then_some((s.0 - dx, s.0 + dx))
        })
        .collect::<Vec<_>>();

    intervals.sort_by_key(|p| p.0);

    let mut combined = vec![intervals[0]];
    for r in intervals.into_iter().skip(1)
    {
        let mut last = combined.last_mut().unwrap();
        if r.0 > last.1
        {
            combined.push(r);
        }
        else
        {
            last.1 = std::cmp::max(last.1, r.1);
        }
    }

    combined.into_iter().map(|r| r.1 - r.0).sum()
}


fn task_two(input: &[String]) -> isize
{
    let input: Vec<_> = input
        .iter()
        .map(|line| parse(line))
        .map(|(s, b)| [s.0, s.1, b.0, b.1])
        .collect::<Vec<_>>();

    let beacon_dist = input
        .iter()
        .map(|[a, b, c, d]| (a - c).abs() + (b - d).abs())
        .collect::<Vec<_>>();

    let xl = [0, 0];
    let xh = [4000000, 4000000];
    let mut stack = vec![[xl, xh]];

    loop
    {
        let [xl, xh] = stack.pop().unwrap();
        if xl == xh
        {
            return xl[0] * 4_000_000 + xl[1];
        }

        let xm = [(xl[0] + xh[0]) / 2, (xl[1] + xh[1]) / 2];

        let mut vert = xl
            .into_iter()
            .zip(xm.into_iter())
            .zip(xh.into_iter())
            .map(|((l, m), h)| [(l, m), (m + 1, h)]);

        let v1 = vert.next().unwrap();
        let v2 = vert.next().unwrap();

        // Cartesian product
        let mut vertices = v1.into_iter().flat_map(|x| v2.into_iter().flat_map(move |y| [x, y]));

        while let (Some(_xl), Some(_xh)) = (vertices.next(), vertices.next())
        {
            // Transpsoe
            let xl = [_xl.0, _xh.0];
            let xh = [_xl.1, _xh.1];

            let fst = input.iter().map(|[x, y, ..]| [(xh[0] - x).max(0), (xh[1] - y).max(0)]);

            let snd = input.iter().map(|[x, y, ..]| [(x - xl[0]).max(0), (y - xl[1]).max(0)]);

            let dist = fst.zip(snd).map(|(a, b)| a[0] + b[0] + a[1] + b[1]);
            let b = !dist.zip(beacon_dist.iter()).any(|(a, b)| a <= *b);
            if b
            {
                stack.push([xl, xh]);
            }
        }
    }
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
