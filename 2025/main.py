from z3 import *
import re

parsed = []

with open("input", "r") as file:
    for line in file:
        line = line.strip()
        if not line:
            continue

        paren_groups = re.findall(r"\((.*?)\)", line)
        paren_lists = [list(map(int, group.split(","))) for group in paren_groups]

        brace_group = re.search(r"\{(.*?)\}", line)
        brace_list = list(map(int, brace_group.group(1).split(",")))

        parsed.append((paren_lists, brace_list))

for p in parsed:
    print(p)


from z3 import *

def solve_sparse_system(cols, target, non_negative=True):
    num_cols = len(cols)
    num_rows = max((r for col in cols for r in col), default=-1) + 1

    if len(target) != num_rows:
        raise ValueError("target length does not match inferred number of rows")

    xs = [Int(f"x{i}") for i in range(num_cols)]

    opt = Optimize()

    for r in range(num_rows):
        involved_cols = [i for i, col in enumerate(cols) if r in col]
        opt.add(Sum(xs[i] for i in involved_cols) == target[r])

    if non_negative:
        for x in xs:
            opt.add(x >= 0)

    total_sum = Sum(xs)
    opt.minimize(total_sum)

    if opt.check() == sat:
        m = opt.model()
        solution = [m[x].as_long() for x in xs]
        return solution
    else:
        return None


