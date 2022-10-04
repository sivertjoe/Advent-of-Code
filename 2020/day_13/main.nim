import strutils
import sequtils
import std/enumerate
import std/monotimes
from times import inMilliseconds
import std/strformat
import sugar

proc read_input(): (int, seq[(int, int)]) =
    let content = readFile("input");
    let spl = content.split('\n');
    var res: seq[(int, int)] = @[];

    for (i, elem) in enumerate(spl[1].split(',')):
        if elem != "x":
            let val = parseInt(elem);
            res.add((val - i, val));
            
    (parseInt(spl[0][0..^1]), res)
    
proc part_one(num: int, list: seq[int]): int =
    var i = num;
    
    while true:
        for elem in list:
            if i mod elem == 0:
                return (i - num) * elem;
        i += 1;
        
func z(a, b: int): int =
    var (a, b) = (a, b);
    let b0 = b;
    var (x0, x1) = (0, 1);
    while a > 1:
        let q = a div b;
        a = a mod b;
        swap a, b;
        x1 = x1 - q * x0;
        swap x0, x1;
        if x1 < 0:
            x1 += b0;
    x1

proc part_two(list: seq[(int, int)]): int =
    var N = list.map(proc(x: (int, int)): int = x[1]).foldl(a * b, 1);
    var sum  = 0;
    for (ai, ni) in list:
        let p = N div ni;
        sum += ai  * z(p, ni) * p;
    
    # Nim mod is kind of weird
    ((sum mod N) + N) mod N

proc time[T](f: (seq[T]) -> int, list: seq[T]) =
    let t0 = getMonotime();
    let res = f(list);
    let diff = (getMonotime() - t0).inMilliseconds;
    echo fmt"({diff}ms)    {res}";


let (num, list) = read_input();

let ll = list.map(proc(x: (int, int)): int = x[1])
let f = proc(list: seq[int]): int =
    part_one(num, list)

time(f, ll)
time(part_two, list)
