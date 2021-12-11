#!/usr/bin/python3

filename = "2021-12-09.dat"


def follow(heightmap, pos, basin):
    for offset in ((-1, 0), (1, 0), (0, -1), (0, 1)):
        neighbour = (pos[0] + offset[0], pos[1] + offset[1])
        h = heightmap.get(neighbour, 0)
        if h < 9 and h > heightmap[pos]:
            basin.add(neighbour)
            follow(heightmap, neighbour, basin)


def part_one():
    heightmap = {}
    with open(filename, "r") as f:
        for i, line in enumerate(f.readlines()):
            for j, height in enumerate(line.strip()):
                heightmap[(i, j)] = int(height)

    risk = 0
    basins = [1, 1, 1]
    for pos, height in heightmap.items():
        minh = min(
            heightmap.get((pos[0] + offset[0], pos[1] + offset[1]), 9)
            for offset in ((-1, 0), (1, 0), (0, -1), (0, 1))
        )
        if height >= minh:
            continue

        basin = set([pos])
        follow(heightmap, pos, basin)
        print("pos", pos, "height", height, "basin", len(basin))
        risk += height + 1
        basins.append(len(basin))

    basins.sort(reverse=True)
    print("risk", risk)
    print("basins", basins[0] * basins[1] * basins[2])


part_one()
