fn read_input<T, P>(path: P) -> Vec<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    std::io::BufReader::new(file)
        .lines()
        .flatten()
        .map(|line| line.parse::<T>().unwrap())
        .collect()
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

fn task_one(input: &[String]) -> i32
{
    calc_magnitude(&addition(input))
}

fn task_two(input: &[String]) -> i32
{
    let calc = |n1: &str, n2: &str| calc_magnitude(&addition(&[n1.to_owned(), n2.to_owned()]));

    let mut max = 0;
    for n1 in input
    {
        for n2 in input
        {
            if n1 == n2
            {
                continue;
            }
            max = std::cmp::max(max, calc(n1, n2));
        }
    }
    max
}

#[inline]
fn number(c: char) -> bool
{
    c != '[' && c != ']' && c != ','
}

#[inline]
fn not_number(c: char) -> bool
{
    !number(c)
}

fn find_explosion(s: &str) -> Option<usize>
{
    let mut depth = 0;

    for (i, c) in s.char_indices()
    {
        if c == '['
        {
            depth += 1;
        }
        else if c == ']'
        {
            depth -= 1;
        }

        if depth == 5
        {
            return Some(i);
        }
    }
    None
}

fn find_split(s: &str) -> Option<(usize, usize)>
{
    for (i, c) in s.char_indices()
    {
        if number(c)
        {
            let right = i + s[i..].find(not_number).unwrap();

            let num = s[i..right].parse::<i32>().unwrap();
            if num >= 10
            {
                return Some((i, right));
            }
        }
    }
    None
}

fn split(s: &str) -> String
{
    let mut s = s.to_string();
    if let Some((l, r)) = find_split(&s)
    {
        let num = s[l..r].parse::<i32>().unwrap();
        let left = (num as f32 / 2.0).floor() as i32;
        let right = (num as f32 / 2.0).ceil() as i32;

        s.replace_range(l..r, &format!("[{},{}]", left, right));
    }
    s
}

fn get_number(s: &str, idx: usize) -> (u32, usize)
{
    let right = idx + s[idx..].find(not_number).unwrap();
    let num = s[idx..right].parse().unwrap();
    (num, right)
}

fn get_numberr(s: &str, idx: usize) -> (u32, usize)
{
    let left = s[..idx].rfind(not_number).unwrap() + 1;
    let num = s[left..=idx].parse().unwrap();
    (num, left)
}

fn explode(s: &str) -> String
{
    let mut s = s.to_string();
    if let Some(idx) = find_explosion(&s)
    {
        let (left, left_idx) = get_number(&s, idx + 1);
        let (right, _) = get_number(&s, left_idx + 1);

        let right_idx = idx + s[idx..].find(']').unwrap();

        s.replace_range(idx..=right_idx, "0");

        // Right side
        if let Some(r_index) = s[idx + 1..].find(number)
        {
            let left_idx = idx + 1 + r_index;
            let (num, right_idx) = get_number(&s, left_idx);
            let ch = format!("{}", num + right);

            s.replace_range(left_idx..right_idx, &ch);
        }
        if let Some(l_index) = s[..idx].rfind(number)
        {
            let (num, left_idx) = get_numberr(&s, l_index);
            let ch = format!("{}", num + left);

            let num_len = format!("{}", num).len();
            s.replace_range(left_idx..left_idx + num_len, &ch);
        }
    }

    s
}

fn reduce(s: String) -> String
{
    let mut res = s;
    loop
    {
        let mut temp = explode(&res);
        while temp != res
        {
            res = temp;
            temp = explode(&res);
        }

        // Done exploding
        temp = split(&res);
        if temp != res
        {
            res = temp;
            continue;
        }

        res = temp;
        break;
    }
    res
}

fn addition(input: &[String]) -> String
{
    let add = |acc: String, x: &String| reduce(join_string(&acc, x));
    input.iter().skip(1).fold(input[0].clone(), add)
}

fn calc_magnitude(input_str: &str) -> i32
{
    fn _calc_magnitude(s: &str) -> (i32, &str)
    {
        if let Some(s) = s.strip_prefix('[')
        {
            let (left, s) = _calc_magnitude(s);
            let idx = s.find(|c| c != ']' && c != ',').unwrap();
            let s = &s[idx..];

            let (right, s) = _calc_magnitude(s);
            (3 * left + 2 * right, s)
        }
        else
        {
            let piv = s.find(|c| c == ',' || c == ']').unwrap();

            let number = s[..piv].parse::<i32>().unwrap();
            (number, &s[piv + 1..])
        }
    }

    _calc_magnitude(input_str).0
}

fn join_string(p1: &str, p2: &str) -> String
{
    format!("[{},{}]", p1, p2)
}

#[cfg(test)]
mod test
{
    use super::*;

    #[test]
    fn test_explode_right()
    {
        let s = "[[[[[9,8],1],2],3],4]";
        let ans = "[[[[0,9],2],3],4]";

        assert_eq!(&explode(s), ans);
    }

    #[test]
    fn test_explode_left()
    {
        let s = "[7,[6,[5,[4,[3,2]]]]]";
        let ans = "[7,[6,[5,[7,0]]]]";

        assert_eq!(&explode(s), ans);
    }

    #[test]
    fn test_explode_left_right()
    {
        let s = "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]";
        let ans = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]";

        assert_eq!(&explode(s), ans);
    }

    #[test]
    fn test_split()
    {
        let p = "[[[[0,7],4],[15,[0,13]]],[1,1]]";
        let p = split(p);
        let ans = "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]";

        assert_eq!(&p, ans);
    }

    #[test]
    fn test_example1()
    {
        let p = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]";
        let p = explode(&split(&split(&explode(&explode(p)))));
        let ans = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]";

        assert_eq!(&p, ans);
    }

    #[test]
    fn test_reduce1()
    {
        let p = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]".to_owned();
        let p = reduce(p);
        let ans = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]";

        assert_eq!(&p, ans);
    }


    #[test]
    fn test_example_addition()
    {
        let vec: Vec<_> = vec![
            "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
            "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
            "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
            "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
            "[7,[5,[[3,8],[1,4]]]]",
            "[[2,[2,2]],[8,[8,1]]]",
            "[2,9]",
            "[1,[[[9,3],9],[[9,0],[0,7]]]]",
            "[[[5,[7,4]],7],1]",
            "[[[[4,2],2],6],[8,7]]",
        ]
        .into_iter()
        .map(|s| s.to_string())
        .collect();

        let p = addition(vec.as_slice());
        let ans = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]";

        assert_eq!(&p, ans);
    }


    #[test]
    fn test_join_strig()
    {
        let p1 = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]";
        let p2 = "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]";
        let p = join_string(p1, p2);
        let ans = "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]";

        assert_eq!(&p, ans);
    }

    #[test]
    fn test_calc_magnitude()
    {
        let n = "[[1,2],[[3,4],5]]";
        assert_eq!(calc_magnitude(n), 143);

        let n = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]";
        assert_eq!(calc_magnitude(n), 1384);

        let n = "[[[[1,1],[2,2]],[3,3]],[4,4]]";
        assert_eq!(calc_magnitude(n), 445);

        let n = "[[[[3,0],[5,3]],[4,4]],[5,5]]";
        assert_eq!(calc_magnitude(n), 791);

        let n = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]";
        assert_eq!(calc_magnitude(n), 3488);
    }
}
