#!/usr/bin/python3
from collections import Counter
filename = 'q11.dat'


def cell_update(g, i, j):
    occupied = 0
    for u in [-1, 0, 1]:
        for v in [-1, 0, 1]:
            if u == 0 and v == 0:
                continue
            n, m = i, j
            seen = '.'
            while seen == '.':
                n += u
                m += v
                if n < 0 or n >= len(g) or m < 0 or m >= len(g[0]):
                    break
                seen = g[n][m]
            if seen == '#':
                occupied += 1

    if g[i][j] == 'L' and occupied == 0:
        return '#'
    elif g[i][j] == '#' and occupied >= 5:
        return 'L'
    return g[i][j]


def grid_update(g):
    n = []
    for l in g:
        n.append(l.copy())

    for i in range(len(g)):
        for j in range(len(g[0])):
            n[i][j] = cell_update(g, i, j)
    return n


with open(filename, 'r') as f:
    grid = [list(l.rstrip()) for l in f.readlines()]

cnt = 0
grid_next = []
while grid_next != grid:
    cnt += 1
    grid_next = grid
    grid = grid_update(grid)
    #for l in grid:
    #    print(''.join(l))
    #print()

print('rounds', cnt)
cells = ''
for l in grid:
    print(''.join(l))
    cells += ''.join(l)
print('occupied cells', Counter(cells)['#'])