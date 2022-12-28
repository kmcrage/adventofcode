#!/usr/bin/python3
# shortest route algorithm
# https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
import collections
import functools
import heapq
import itertools

data_filename = "2022-12-16.dat"
# data_filename = "test.dat"


def parse(filename):
    valves = {}
    with open(filename, "r") as f:
        for line in f:
            tokens = line.split()
            valves[tokens[1]] = {
                "rate": int(tokens[4][5:-1]),
                "tunnels": sorted([t.strip(",") for t in tokens[9:]]),
            }

    return valves


def routes(valves):
    routes = collections.defaultdict(lambda: len(valves))
    for v in valves:
        routes[(v, v)] = 0
        for w in valves[v]["tunnels"]:
            routes[(v, w)] = 1
    for i, v, w in itertools.product(valves, repeat=3):
        routes[(v, w)] = min(routes[(v, w)], routes[(v, i)] + routes[(i, w)])
    return routes


VALVES = parse(data_filename)
ROUTES = routes(VALVES)


@functools.lru_cache(maxsize=None)
def get_estimax(time, locations, moves, part_two):
    rates = [VALVES[l]["rate"] for l in locations]
    rates.sort(reverse=True)
    times = range(time - 1, 0, -1)
    if part_two:
        times = list(times)
        times += range(moves - 1)
        times.sort(reverse=True)
    return sum(r * t for r, t in zip(rates, times))


@functools.lru_cache(maxsize=None)
def single(start, moves, unvisited, part_two=False):
    # queue = [(estimax, time, pos, score, remaining), ...] which we keep sorted
    queue = [
        (get_estimax(moves, unvisited, moves, part_two), moves, start, 0, unvisited)
    ]
    best = 0
    opened = 0
    if part_two:
        best, min_opened_valves = single(start, moves, unvisited)
    while queue:
        estimax, time, pos, score, remaining = heapq.heappop(queue)
        if estimax <= best:
            continue

        if part_two and len(remaining) < len(unvisited) - min_opened_valves:
            elephant, e_opened = single(start, moves, remaining)
            if elephant + score > best:
                best = elephant + score
                opened = len(unvisited) - len(remaining) + e_opened

        for nhbr in remaining:
            nhbr_time = time - ROUTES[(pos, nhbr)] - 1
            if nhbr_time <= 0:
                continue
            nhbr_score = score + nhbr_time * VALVES[nhbr]["rate"]
            if nhbr_score > best:
                best = nhbr_score
                opened = len(unvisited) - len(remaining)
            nhbr_remaining = remaining - {nhbr}
            nhbr_estimax = nhbr_score + get_estimax(
                nhbr_time, nhbr_remaining, moves, part_two
            )
            if nhbr_estimax > best:
                heapq.heappush(
                    queue, (nhbr_estimax, nhbr_time, nhbr, nhbr_score, nhbr_remaining)
                )
    return best, opened


def part_one(start, moves, part_two=False):
    active = frozenset(v for v in VALVES if VALVES[v]["rate"] != 0)
    result, opened = single(start, moves, active, part_two=part_two)
    print(f"{moves} moves: {result}, {len(active)} active nodes, {opened} open valves")


part_one("AA", 30)
part_one("AA", 26, part_two=True)
