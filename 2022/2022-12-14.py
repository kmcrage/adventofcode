#!/usr/bin/python3
import functools

data_filename = "2022-12-14.dat"
# data_filename = "test.dat"


def parser(filename):  # sourcery skip: use-itertools-product
    sand = set()
    with open(filename, "r") as f:
        for line in f:
            tokens = line.strip().split()
            while len(tokens) > 2:
                pos = [
                    [int(n) for n in tokens[0].split(",")],
                    [int(n) for n in tokens[2].split(",")],
                ]
                pos.sort()
                for i in range(pos[0][0], pos[1][0] + 1):
                    for j in range(pos[0][1], pos[1][1] + 1):
                        sand.add((i, j))
                tokens = tokens[2:]
    return sand


def pour_sand(sand, start):
    depth = max(s[1] for s in sand)
    pos = start
    grains = 0
    while pos[1] <= depth and start not in sand:
        for new_pos in (
            (pos[0], pos[1] + 1),
            (pos[0] - 1, pos[1] + 1),
            (pos[0] + 1, pos[1] + 1)
        ):
            if new_pos not in sand:
                pos = new_pos
                break
        else:
            sand.add(pos)
            grains += 1
            pos = start
    print("grains:", grains)

def add_floor(sand):
    floor_depth = max(s[1] for s in sand) + 2
    sand_min = min(s[0] for s in sand) - floor_depth
    sand_max = max(s[0] for s in sand) + floor_depth
    for x in range(sand_min, sand_max ):
        sand.add((x, floor_depth))
    
sand = parser(data_filename)
print('no floor ', end='')
pour_sand(sand.copy(), (500, 0))
print('add floor ', end='')
add_floor(sand)
pour_sand(sand.copy(), (500, 0))

