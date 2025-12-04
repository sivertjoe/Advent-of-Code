#import "Basic";
#import "String";
#import "File";

Tile :: enum {
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

neighbors :: (tiles: [..][..]Tile, y: int, x: int) -> [][2]int {
    ns: [..][2]int;

    for yy : -1..1 {
        for _xx : -1..1 {
            if yy == 0 && _xx == 0 continue;

            ny := y + yy;
            nx := x + _xx;
            if ny >= 0 && ny < tiles.count && nx >= 0 && nx < tiles[0].count {
                array_add(*ns, int.[ny, nx]);
            }
        }
    }

    return ns;
}
count_neightbors_papers :: (tiles: [..][..]Tile, y: int, x: int) -> int {
    ns := neighbors(tiles, y, x);
    // print("ns = %\n", ns);
    count := 0;
    for ns {
        y := it[0];
        x := it[1];

        if tiles[y][x] == Tile.Paper {
            count += 1;
        }
        // print("tile[%][%] = %\n", y, x, tiles[y][x]);
    }
    return count;
}

task_one :: (lines: []string) -> int {
    tiles := parse(lines);

    debug: [..][2]int;

    // print("count = %\n", count_neightbors_papers(tiles, 0, 2));
    // return 0;
    count := 0;
    for line, y : tiles {
        for tile, x : line {
            if tile == Tile.Paper && count_neightbors_papers(tiles, y, x) < 4 {
                // print("% %\n", y, x);
                count += 1;
                array_add(*debug, int.[y, x]);
            }
        }
    }

    for line, y : tiles {
        for tile, x : line {
            if tile == Tile.Paper {
                for debug {
                    if y == it[0] && x == it[1] {
                        print("x");
                        continue tile;
                    }
                }
                print("@");
            } else {
                print(".");
            }
        }
        print("\n");
    }

    for tiles array_free(it);
    array_free(tiles);
    return count;
}


task_two :: (lines: []string) -> int {
    tiles := parse(lines);

    debug: [..][2]int;

    // print("count = %\n", count_neightbors_papers(tiles, 0, 2));
    // return 0;
    count := 0;
    for line, y : tiles {
        for tile, x : line {
            if tile == Tile.Paper && count_neightbors_papers(tiles, y, x) < 4 {
                // print("% %\n", y, x);
                count += 1;
                array_add(*debug, int.[y, x]);
            }
        }
    }

    for line, y : tiles {
        for tile, x : line {
            if tile == Tile.Paper {
                for debug {
                    if y == it[0] && x == it[1] {
                        print("x");
                        continue tile;
                    }
                }
                print("@");
            } else {
                print(".");
            }
        }
        print("\n");
    }

    for tiles array_free(it);
    array_free(tiles);
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
