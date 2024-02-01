import math, random

V = set()
E = set()

for line in open('input'):
    v, *ws = line.replace(':',' ').split()
    V |= {v, *ws}
    E |= {(v,w) for w in ws}


ss = lambda v: next(s for s in subsets if v in s)

C = 0

while True:
    subsets = [{v} for v in V]
    
    while len(subsets) > 2:
        C += 1
        s1, s2 = map(ss, random.choice([*E]))
        if C < 20:
            print(len(s1))

        if s1 != s2:
            s1 |= s2; 
            subsets.remove(s2)
    if sum(ss(u) != ss(v) for u,v in E) < 4:
        break

print(math.prod(map(len, subsets)))
