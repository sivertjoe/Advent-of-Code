#import "Basic";
#import "String";
#import "File";

Tile :: enum u8 {
    Paper;
    Empty;
}

parse :: (lines: []string) -> [..][..]Tile {
    tiles: [..][..]Tile;

    for line : lines {
        if line == "" continue;
        arr: [..]Tile;

        for ch : line {
            if ch == #char "." {
                array_add(*arr, Tile.Empty);
            } else if ch == #char "@" {
                array_add(*arr, Tile.Paper);
            }
        }

        array_add(*tiles, arr);
    }

    return tiles;
}

neighbors :: (tiles: [..][..]Tile, y: int, x: u8) -> [][2]u8 {
    ns: [..][2]u8;
    ns.allocator = temp;

    for yy : -1..1 {
        for _xx : -1..1 {
            if yy == 0 && _xx == 0 continue;

            ny := y + yy;
            nx := x + _xx;
            if ny >= 0 && ny < tiles.count && nx >= 0 && nx < tiles[0].count {
                array_add(*ns, u8.[xx ny, xx nx]);
            }
        }
    }

    return ns;
}

count_neightbors_papers :: (tiles: [..][..]Tile, y: u8, x: u8) -> int {
    ns := neighbors(tiles, y, x);
    count := 0;
    for ns {
        if tiles[it[0]][it[1]] == Tile.Paper {
            count += 1;
        }
    }
    return count;
}

mark_to_remove :: (tiles: [..][..]Tile) -> [..][2]u8 {
    marked: [..][2]u8;
    marked.allocator = temp;

    for line, y : tiles {
        for tile, x : line {
            if tile == Tile.Paper && count_neightbors_papers(tiles, xx y, xx x) < 4 {
                array_add(*marked, u8.[xx y, xx x]);
            }
        }
    }

    return marked;
}

task_one :: (lines: []string) -> int {
    tiles := parse(lines);
    defer {
        for tiles array_free(it);
        array_free(tiles);
    }

    marked := mark_to_remove(tiles);
    return marked.count;
}

task_two :: (lines: []string) -> int {
    tiles := parse(lines);
    defer {
        for tiles array_free(it);
        array_free(tiles);
    }

    count := 0;
    while true {
        marked := mark_to_remove(tiles);
        if marked.count == 0 break;

        count += marked.count;
        for marked {
            tiles[it[0]][it[1]] = Tile.Empty;
        }
        reset_temporary_storage();
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

    time(arg, .One, task_one);
    time(arg, .Two, task_two);
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
