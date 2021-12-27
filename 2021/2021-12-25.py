#!/usr/bin/python3
import copy

filename = "2021-12-25.dat"

grid = []
with open(filename, "r") as f:
    for l in f.readlines():
        grid.append(list(l.strip()))

for row in grid:
    print("".join(row))
print()

updated = True
steps = 0
while updated:
    steps += 1
    updated = False

    # east >
    prev = copy.deepcopy(grid)
    for r, row in enumerate(prev):
        for c, col in enumerate(row):
            if col != ">":
                continue
            cc = (c + 1) % len(row)
            if row[cc] == ".":
                grid[r][c] = "."
                grid[r][cc] = ">"
                updated = True


    prev = copy.deepcopy(grid)
    # south >
    for r, row in enumerate(prev):
        for c, col in enumerate(row):
            if col != "v":
                continue
            rr = (r + 1) % len(prev)
            if prev[rr][c] == ".":
                grid[r][c] = "."
                grid[rr][c] = "v"
                updated = True

for row in grid:
    print("".join(row))

print("steps:", steps)
