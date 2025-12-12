#import "Basic";
#import "String";
#import "File";

Tree :: struct {
    dim_x: int;
    dim_y: int;
    counts: [..]int;
}

Shape :: struct {
    area: int;
}

parse :: (lines: []string) -> [..]Shape, [..]Tree {
    shapes: [..]Shape;
    trees: [..]Tree;

    i := 0;
    while true {
        if contains(lines[i], "x") break;
        idx := lines[i][0] - (#char "0");
        area :=  replace(lines[i+1], ".", "").count
               + replace(lines[i+2], ".", "").count
               + replace(lines[i+3], ".", "").count;
        array_add(*shapes, Shape.{area});
        i += 5;
    }

    while true {
        if i >= lines.count || lines[i] == "" break;

        spl := split(lines[i], " ");
        dim := spl[0];
        dim = slice(dim, 0, dim.count-1);
        dim_split := split(dim, "x");
        x := string_to_int(dim_split[0]);
        y := string_to_int(dim_split[1]);

        counts: [..]int;
        for array_view(spl, 1) {
            array_add(*counts, string_to_int(it));
        }
        tree := Tree.{x, y, counts};
        array_add(*trees, tree);
        i += 1;
    }

    return shapes, trees;
}

task_one :: (lines: []string) -> int {
    shapes, trees := parse(lines);
    count := 0;

    for tree : trees {
        m := 0;
        for count : tree.counts {
            m += shapes[it_index].area * count;
        }
        if m <= tree.dim_x * tree.dim_y count += 1;
    }

    return count;
}

main :: () {
    args := get_command_line_arguments();
    input := ifx args.count >= 2 then args[1] else "input";

    text, ok := read_entire_file(input);
    if !ok {
        print("Missing file %\n", input);
        exit(0);
    }
    arg := split(text, "\n");
    defer array_free(arg);
    for * arg {
        if <<it == "" {
            continue;
        }
        new := trim(<<it);
        <<it = new;
    }

    context.allocator = temp;
    time(arg, .One, task_one);
}


time :: (input: []string, task: Task, fn: ([]string) -> $U) {
    start := current_time_monotonic();
    res := fn(input);
    duration := operator - (current_time_monotonic(), start);
    elapsed := duration.low / 1000_000_000_000;

    if #complete task == {
        case .One;
            print("(%ms)\tTask one: \x1b[0;34;34m%\x1b[0m\n", elapsed, res);
        case .Two;
            print("(%ms)\tTask two: \x1b[0;33;10m%\x1b[0m\n", elapsed, res);
    }
}

Task :: enum {
    One;
    Two;
}
