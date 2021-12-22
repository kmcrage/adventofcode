#!/usr/bin/python3

import functools

filename = "2021-12-22.dat"


def part_one():
    init = [(-50, 50), (-50, 50), (-50, 50)]
    cubes = set()
    with open(filename, "r") as f:
        for line in f.readlines():
            state, vol = line.strip().split()
            volume = [
                v.replace("=", " ").replace(".", " ").split() for v in vol.split(",")
            ]
            for i in range(
                max(int(volume[0][1]), init[0][0]),
                min(int(volume[0][2]), init[0][1]) + 1,
            ):
                for j in range(
                    max(int(volume[1][1]), init[1][0]),
                    min(int(volume[1][2]), init[1][1]) + 1,
                ):
                    for k in range(
                        max(int(volume[2][1]), init[2][0]),
                        min(int(volume[2][2]), init[2][1]) + 1,
                    ):
                        cube = (i, j, k)
                        if state == "on":
                            cubes.add(cube)
                        elif cube in cubes:
                            cubes.remove(cube)
    print("basic part one:", len(cubes))


def parse():
    cubes = []
    with open(filename, "r") as f:
        for line in f.readlines():
            state, vol = line.strip().split()
            volume = [
                v.replace("=", " ").replace(".", " ").split() for v in vol.split(",")
            ]
            cube = tuple([tuple(int(u) for u in v[1:]) for v in volume] + [state])
            cubes.append(cube)
    return tuple(cubes)


def cube_size(cube):
    cnt = 1
    for i in range(3):
        cnt *= cube[i][1] - cube[i][0] + 1
    return cnt


def intersection(cube_a, cube_b):
    return tuple(
        tuple(
            [
                max(cube_a[n][0], cube_b[n][0]),
                min(cube_a[n][1], cube_b[n][1]),
            ]
        )
        for n in range(3)
    )


# everything needs to be tuples for this to work
@functools.lru_cache(maxsize=None)
def count(cube, cubes, init=False):
    if init:
        cube = intersection(cube, [(-50, 50)] * 3)
        if any(cube[n][0] > cube[n][1] for n in range(3)):
            return 0

    cnt = cube_size(cube)

    for i, intersect in enumerate(cubes, 1):
        subregion = intersection(cube, intersect)

        if any(subregion[n][0] > subregion[n][1] for n in range(3)):
            continue
        cnt -= count(subregion, cubes[i:], init=init)
    return cnt


part_one()

cubes = parse()
init = sum(
    count(cube, cubes[c:], init=True)
    for c, cube in enumerate(cubes, 1)
    if cube[3] != "off"
)
print("init", init)
cnt = sum(count(cube, cubes[c:]) for c, cube in enumerate(cubes, 1) if cube[3] != "off")
print("count", cnt)
