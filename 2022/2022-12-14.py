#!/usr/bin/python3
import functools

data_filename = "2022-12-14.dat"
# data_filename = "test.dat"


def parser(filename):  # sourcery skip: use-itertools-product
    wall = set()
    with open(filename, "r") as f:
        for line in f:
            coords = [list(map(int, c.split(','))) for c in line.split(' -> ')]
            for pos in zip(coords, coords[1:]):
                pos = sorted(pos)
                for i in range(pos[0][0], pos[1][0] + 1):
                    for j in range(pos[0][1], pos[1][1] + 1):
                        wall.add((i, j))
    return wall


def drop(pos, depth, sand):
    if pos in sand:
        return False
    if pos[1] >= depth:
        return True
    for new_pos in (
        (pos[0], pos[1] + 1),
        (pos[0] - 1, pos[1] + 1),
        (pos[0] + 1, pos[1] + 1),
    ):
        if drop(new_pos, depth, sand):
            return True
    sand.add(pos)
    return False


def pour_sand(walls, start):
    sand = walls.copy()
    depth = max(s[1] for s in sand)
    drop(start, depth, sand)
    print("grains:", len(sand) - len(walls))


def add_floor(sand):
    floor_depth = max(s[1] for s in sand) + 2
    sand_min = min(s[0] for s in sand) - floor_depth - 2
    sand_max = max(s[0] for s in sand) + floor_depth + 2
    for x in range(sand_min, sand_max):
        sand.add((x, floor_depth))


walls = parser(data_filename)
print("no floor ", end="")
pour_sand(walls, (500, 0))
print("add floor ", end="")
add_floor(walls)
pour_sand(walls, (500, 0))
