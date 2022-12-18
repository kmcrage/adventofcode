#!/usr/bin/python3
import collections
import itertools
import math

data_filename = "2022-12-18.dat"
# data_filename = "test.dat"


def get_volume(filename):
    volume = set()
    with open(filename, "r") as f:
        for line in f:
            volume.add(tuple(map(int, line.split(","))))
    return volume


def count_faces(volume):
    count = 0
    for pos in volume:
        for idx in range(3):
            for dir in (-1, 1):
                cur = list(pos)
                cur[idx] += dir
                if tuple(cur) not in volume:
                    count += 1
    print(f"num faces: {count}")


def count_external_faces(volume, filled):
    count = 0
    for pos in volume:
        for idx in range(3):
            for dir in (-1, 1):
                cur = list(pos)
                cur[idx] += dir
                cur = tuple(cur)
                if filled[cur] == " ":
                    count += 1
    print(f"num external faces: {count}")


def fill(node, volume):
    # '.' is internal, ' ' is external, '#' is rock
    queue = collections.deque()
    queue.append(node)
    while queue:
        cur = queue.popleft()
        if volume[cur] != ".":
            continue
        volume[cur] = " "
        for idx in range(3):
            for dir in (-1, 1):
                nhbr = list(cur)
                nhbr[idx] += dir
                nhbr = tuple(nhbr)
                if nhbr in volume and volume[nhbr] == ".":
                    queue.append(nhbr)


def flood_fill(volume):
    """Create a dictionary one bigger than the lava volume.
    initialise as '# for rock, '.' for non-rock
    The corners are outside, and the outside is connected
    Then flood fill: '.' is internal, ' ' is external, '#' is rock

    Args:
        volume (_type_): _description_

    Returns:
        _type_: _description_
    """
    filled = {}
    minmax = [[math.inf, -math.inf] for _ in range(3)]
    for pos in volume:
        for idx in range(3):
            minmax[idx][0] = min(minmax[idx][0], pos[idx])
            minmax[idx][1] = max(minmax[idx][1], pos[idx])

    for i, j, k in itertools.product(
        range(minmax[0][0] - 1, minmax[0][1] + 2),
        range(minmax[1][0] - 1, minmax[1][1] + 2),
        range(minmax[2][0] - 1, minmax[2][1] + 2),
    ):
        pos = (i, j, k)
        filled[pos] = "#" if pos in volume else "."

    start = (minmax[0][0] - 1, minmax[1][0] - 1, minmax[2][0] - 1)
    fill(start, filled)
    return filled


volume = get_volume(data_filename)

count_faces(volume)

filled_volume = flood_fill(volume)
count_external_faces(volume, filled_volume)
