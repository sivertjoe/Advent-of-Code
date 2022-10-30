use std::collections::*;

fn rotate(s: &String, n: usize) -> String
{
    let mut new = s.clone();

    let size = if s.len() == 9 { 3 } else { 2 };
    for _ in 0..n
    {
        let mut temp = String::new();
        for y in 0..size
        {
            for x in (0..size).rev()
            {
                let idx = (x * size) + y;
                temp.push(new.as_bytes()[idx] as char);
            }
        }
        new = temp;
    }
    new
}

fn flip(s: &String) -> String
{
    let mut new = String::new();
    let size = if s.len() == 9 { 3 } else { 2 };
    for y in (0..size).rev()
    {
        for x in 0..size
        {
            let idx = y * size + x;
            new.push(s.as_bytes()[idx] as char);
        }
    }
    new
}


fn add_line(line: &String, map: &mut HashMap<String, String>)
{
    let (r#in, out) = line.split_once(" => ").unwrap();

    let mut out = out.to_string();
    out.retain(|c| c != '/');

    let mut r#in = r#in.to_string();
    r#in.retain(|c| c != '/');

    let r#in = r#in;

    for i in 0..=3
    {
        let p1 = rotate(&r#in, i);
        let p2 = flip(&p1);
        map.insert(p1, out.to_string());
        map.insert(p2, out.to_string());
    }
}

fn parse(input: &[String]) -> HashMap<String, String>
{
    let mut map = HashMap::new();
    for line in input
    {
        add_line(line, &mut map);
    }
    map
}

fn do_n(n: usize, image: &String, size: usize, rules: &HashMap<String, String>) -> String
{
    let chunks = size / n;
    let chunk_size = n;
    let chunks_per = size / chunk_size;

    let mut vec = Vec::with_capacity(chunks_per * chunks_per);

    for y in 0..chunks_per
    {
        for x in 0..chunks_per
        {
            let mut s = String::with_capacity(chunk_size * chunk_size);
            for yy in 0..chunk_size
            {
                for xx in 0..chunk_size
                {
                    let offset_x = x * chunk_size;
                    let offset_y = y * size * chunk_size;
                    let idx = offset_y + offset_x + xx + (yy * size);
                    s.push(image.as_bytes()[idx] as char);
                }
            }
            vec.push(s);
        }
    }
    let vec = vec.into_iter().map(|s| rules[&s].clone()).collect::<Vec<_>>();

    let chunks2 = if n == 2 { 3 } else { 4 };
    let mut s = String::new();

    for xs in vec.chunks(chunks)
    {
        for y in 0..chunks2
        {
            for i in 0..xs.len()
            {
                for x in 0..chunks2
                {
                    let idx = y * chunks2 + x;
                    s.push(xs[i].as_bytes()[idx] as char);
                }
            }
        }
    }

    s
}

fn solve<const N: usize>(input: &[String]) -> usize
{
    let rules = parse(input);
    let mut image = ".#...####".to_string();
    for _ in 0..N
    {
        let size = (image.len() as f32).sqrt().floor() as usize;
        if size % 2 == 0
        {
            image = do_n(2, &image, size, &rules);
        }
        else
        {
            image = do_n(3, &image, size, &rules);
        }
    }
    image.chars().filter(|ch| *ch == '#').count()
}

fn task_one(input: &[String]) -> usize
{
    solve::<5>(input)
}

fn task_two(input: &[String]) -> usize
{
    solve::<18>(input)
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


#[cfg(test)]
mod test
{
    use super::*;
    #[test]
    fn test_rotate()
    {
        let s = ".#...####".to_string();
        let s2 = rotate(&s, 1);
        let s3 = rotate(&s, 2);
        let s4 = rotate(&s, 3);
        let s5 = rotate(&s, 4);
        assert_eq!(s2, "#..#.###.");
        assert_eq!(s3, "####...#.");
        assert_eq!(s3, "####...#.");
        assert_eq!(s4, ".###.#..#");
        assert_eq!(s5, s);
    }

    #[test]
    fn test_flip()
    {
        let s = ".#...####".to_string();
        let s2 = flip(&s);
        assert_eq!(s2, "###..#.#.");
    }
}
