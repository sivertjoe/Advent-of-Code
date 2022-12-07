use std::{cell::RefCell, collections::*, rc::Rc};


#[derive(Debug)]
enum Type
{
    File(usize),
    Dir(HashMap<String, Rc<RefCell<Node>>>),
}

#[derive(Debug)]
struct Node
{
    name:   String,
    parent: Option<Rc<RefCell<Node>>>,
    typ:    Type,
}

fn parse(input: &[String]) -> (Rc<RefCell<Node>>, Vec<Rc<RefCell<Node>>>)
{
    use Type::*;
    let root = Rc::new(RefCell::new(Node {
        name:   "/".to_string(),
        parent: None,
        typ:    Dir(HashMap::new()),
    }));

    let mut curr = Rc::clone(&root);
    let mut folders = Vec::new();
    folders.push(Rc::clone(&root));

    for line in &input[1..]
    {
        let split = line.split_whitespace().collect::<Vec<_>>();
        if split[0] == "$"
        {
            if split[1] == "cd"
            {
                if split[2] == ".."
                {
                    let parent = Rc::clone(curr.borrow().parent.as_ref().unwrap());
                    curr = parent;
                }
                else if split[2] == "/"
                {
                    curr = Rc::clone(&root);
                }
                else
                {
                    // cd x
                    let name = split[2];
                    let new = {
                        let Dir(ref children) = curr.borrow().typ else { panic!() };
                        let new = children.get(name).unwrap();
                        Rc::clone(new)
                    };

                    curr = new;
                }
            }
        }
        else
        {
            // We can see two things here, a file, or a directory

            let Dir(ref mut children) = curr.borrow_mut().typ else { panic!() };

            // It was a file
            // children.push(split[1].to_string());
            let new_node = if let Ok(size) = split[0].parse::<usize>()
            {
                Rc::new(RefCell::new(Node {
                    name:   split[1].to_string(),
                    parent: Some(curr.clone()),
                    typ:    File(size),
                }))
            }
            else
            {
                assert!(split[0] == "dir");
                let node = Rc::new(RefCell::new(Node {
                    name:   split[1].to_string(),
                    parent: Some(curr.clone()),
                    typ:    Dir(HashMap::new()),
                }));

                folders.push(Rc::clone(&node));
                node
            };
            let name = new_node.borrow().name.clone();
            children.insert(name, new_node);
        }
    }
    (root, folders)
}

fn size(root: Rc<RefCell<Node>>) -> usize
{
    let mut sum = 0;
    let Type::Dir(ref children) = root.borrow().typ else { panic!() };
    for node in children.values()
    {
        match node.borrow().typ
        {
            Type::File(size) =>
            {
                sum += size;
            },
            Type::Dir(_) =>
            {
                sum += size(Rc::clone(node));
            },
        }
    }


    sum
}

fn task_one(input: &[String]) -> usize
{
    let (_, folders) = parse(input);
    folders.into_iter().map(|f| size(f)).filter(|s| *s <= 100000).sum()
}

fn task_two(input: &[String]) -> usize
{
    let (root, folders) = parse(input);

    let mut sizes = folders.iter().map(|f| size(Rc::clone(f))).collect::<Vec<_>>();
    sizes.sort_unstable();

    let total_size = size(Rc::clone(&root));
    let fs_size = 70000000;
    let free_space = 30000000;

    let available_free_space = fs_size - total_size;

    let require = free_space - available_free_space;
    sizes.into_iter().find(|d| *d >= require).unwrap()
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
