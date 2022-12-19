#!/usr/bin/python3
# shortest route algorithm
# https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
import collections
from itertools import chain, combinations
import functools

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
        for w in valves[v]["tunnels"]:
            routes[(v, w)] = 1
    for i in valves:
        for v in valves:
            for w in valves:
                routes[(v, w)] = min(routes[(v, w)], routes[(v, i)] + routes[(i, w)])
    return routes


@functools.lru_cache(maxsize=None)
def travel(pos, moves, unvisited):
    if moves <= 0 or not unvisited:
        return 0

    unvisited = unvisited - {pos}
    pos_score = VALVES[pos]["rate"] * moves
    best = 0
    for tunnel in unvisited:
        score = travel(tunnel, moves - 1 - ROUTES[(pos, tunnel)], unvisited)
        if score > best:
            best = score
    return best + pos_score


def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))


def part_one(moves):
    active = frozenset(v for v in VALVES if VALVES[v]["rate"] != 0)
    result = travel("AA", moves, active)
    print(f"part one {moves}: {result}")


def part_two(moves):
    active = frozenset(v for v in VALVES if VALVES[v]["rate"] != 0)
    result = 0
    for human in powerset(active):
        if len(active) <= 2 * len(human):
            continue
        human = frozenset(human)
        elephant = active - human
        score = travel("AA", moves, human)
        elephant_est = sum(
            r * m
            for r, m in zip(
                sorted([VALVES[v]["rate"] for v in elephant], reverse=True),
                range(moves, 0, -1),
            )
        )
        if score + elephant_est < result:
            continue
        score += travel("AA", moves, elephant)
        if score > result:
            result = score
    print(f"part {moves}: {result}")


VALVES = parse(data_filename)
ROUTES = routes(VALVES)
part_one(30)
part_two(26)
