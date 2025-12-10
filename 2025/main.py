from z3 import *

# Integer variables
x1, x2, x3, x4, x5, x6 = Ints("x1 x2 x3 x4 x5 x6")

s = Solver()

# Equations from the vector sum
s.add(x5 + x6 == 3)
s.add(x2 + x6 == 5)
s.add(x3 + x4 + x5 == 4)
s.add(x1 + x2 + x4 == 7)

# Optional: restrict to non-negative coefficients
for x in [x1, x2, x3, x4, x5, x6]:
    s.add(x >= 0)

if s.check() == sat:
    m = s.model()
    print("Solution:")
    print("x1 =", m[x1])
    print("x2 =", m[x2])
    print("x3 =", m[x3])
    print("x4 =", m[x4])
    print("x5 =", m[x5])
    print("x6 =", m[x6])
else:
    print("No solution")
