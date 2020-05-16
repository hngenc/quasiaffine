#!/usr/bin/env python3

def div(x, y):
    return x // y

def mod(x, y):
    return x % y

c = 3
doriginals = []
moriginals = []
for i in range(0, 1, 1):
    for j in range(0, 8, 2):
        expr = mod(j, 4)
        d = div(expr, c)
        m = mod(expr, c)
        doriginals.append(d)
        moriginals.append(m)

dopts = []
mopts = []
for i in range(0, 1, 1):
    for j in range(0, 4, 1):
        d = 0
        m = mod(2 * j, 4)
        dopts.append(d)
        mopts.append(m)

if doriginals == dopts and moriginals == mopts:
    print("Success!")
else:
    print("Failure")

print(doriginals)
print(dopts)
print(moriginals)
print(mopts)

