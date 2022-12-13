#!/usr/bin/python3
# Dijstra https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
# and heapq to avoid expensive sorts

import heapq

data_filename = "2022-12-12.dat"
# data_filename = "test.dat"


def parser(filename):
    heights = {}
    start = (0, 0)
    end = (0, 0)
    with open(filename, "r") as f:
        for i, line in enumerate(f.readlines()):
            for j, height in enumerate(line.strip()):
                pos = (i, j)
                if height == "S":
                    start = pos
                    heights[pos] = 0
                elif height == "E":
                    end = pos
                    heights[pos] = ord("z") - ord("a")
                else:
                    heights[pos] = ord(height) - ord("a")
    return heights, start, end


def get_neighbours(heights, pos):
    for offset in ((1, 0), (0, 1), (-1, 0), (0, -1)):
        nhbr = (pos[0] + offset[0], pos[1] + offset[1])
        if nhbr not in heights or heights[pos] - 1 > heights[nhbr]:
            continue
        yield nhbr


def dijkstra(heights, start, end):
    visited = set()
    result = {start: 0}
    # candidates = [(risk, pos), ...] which we keep sorted
    candidates = [(0, start)]
    part_two = None
    while end not in visited:
        pos = heapq.heappop(candidates)[1]
        if pos in visited:
            continue

        for nghbr in get_neighbours(heights, pos):
            if nghbr in visited:
                continue
            distance = result[pos] + 1
            if nghbr not in result or distance < result[nghbr]:
                result[nghbr] = distance
                heapq.heappush(candidates, (distance, nghbr))

        visited.add(pos)
        if not part_two and not heights[pos]:
            part_two = pos

    print("part one result:", result[end])
    print("part two result:", result[part_two])


heights, start, end = parser(data_filename)
dijkstra(heights, end, start)
