#!/usr/bin/python3
import collections
import math

data_filename = "2022-12-18.dat"
# data_filename = "test.dat"


def get_lava(filename):
    lava = set()
    with open(filename, "r") as f:
        for line in f:
            lava.add(tuple(map(int, line.split(","))))
    return lava


def count_faces(lava, water=None):
    count = 0
    for pos in lava:
        for idx in range(3):
            for dir in (-1, 1):
                cur = list(pos)
                cur[idx] += dir
                cur = tuple(cur)
                if (not water and cur not in lava) or (water and cur in water):
                    count += 1
    print(f"faces: {count}")


def flood_fill(lava):
    minmax = [[math.inf, -math.inf] for _ in range(3)]
    for pos in lava:
        for idx in range(3):
            minmax[idx][0] = min(minmax[idx][0], pos[idx])
            minmax[idx][1] = max(minmax[idx][1], pos[idx])

    start = (minmax[0][0] - 1, minmax[1][0] - 1, minmax[2][0] - 1)

    water = set()
    queue = collections.deque([start])
    while queue:
        cur = queue.popleft()
        if cur in water or cur in lava:
            continue
        water.add(cur)
        for idx in range(3):
            for dir in (-1, 1):
                nhbr = list(cur)
                nhbr[idx] += dir
                if nhbr[idx] < minmax[idx][0] - 1 or nhbr[idx] > minmax[idx][1] + 1:
                    continue
                nhbr = tuple(nhbr)
                if nhbr not in water and nhbr not in lava:
                    queue.append(nhbr)

    return water


lava = get_lava(data_filename)
print("all ", end="")
count_faces(lava)
water = flood_fill(lava)
print("external ", end="")
count_faces(lava, water=water)
