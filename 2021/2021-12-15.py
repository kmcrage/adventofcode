#!/usr/bin/python3
# Dijstra https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
# and heapq to avoid expensive sorts

import heapq

filename = "2021-12-15.dat"


def parser():
    risks = {}
    end = (0, 0)
    with open(filename, "r") as f:
        for i, line in enumerate(f.readlines()):
            for j, risk in enumerate(line.strip()):
                end = (i, j)
                risks[end] = int(risk)
    return risks, end


def get_neighbours(pos):
    for offset in ((1, 0), (0, 1), (-1, 0), (0, -1)):
        yield (pos[0] + offset[0], pos[1] + offset[1])


def repeater(risks, end, repeats):
    scaled = {}
    final = (0, 0)
    for pos, val in risks.items():
        for irepeat in range(repeats):
            for jrepeat in range(repeats):
                rpos = (
                    pos[0] + (end[0] + 1) * irepeat,
                    pos[1] + (end[1] + 1) * jrepeat,
                )
                scaled[rpos] = (val + irepeat + jrepeat - 1) % 9 + 1
                final = tuple(max(rpos[i], final[i]) for i in (0, 1))

    return scaled, final


def dijkstra(risks, end):
    visited = set()
    result = {(0, 0): 0}
    # candidates = [(risk, pos), ...] which we keep sorted
    candidates = [(0, (0, 0))]
    while len(visited) < len(risks):
        pos = heapq.heappop(candidates)[1]
        if pos in visited:
            continue

        for nghbr in get_neighbours(pos):
            if nghbr in visited or nghbr not in risks:
                continue
            risk = result[pos] + risks[nghbr]
            if nghbr not in result or risk < result[nghbr]:
                result[nghbr] = risk
                heapq.heappush(candidates, (risk, nghbr))

        visited.add(pos)
        if pos == end:
            break

    print("result:", result[end], "(visited:", len(visited), ")")


risks, end = parser()
dijkstra(risks, end)
risks, end = repeater(risks, end, 5)
dijkstra(risks, end)
