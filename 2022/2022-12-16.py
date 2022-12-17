#!/usr/bin/python3
# shortest route algorithm
# https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
import collections
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
def travel(pos, moves, unvisited, part):
    if moves <= 0:
        return travel("AA", part[0], unvisited, part[1:]) if part else 0

    unvisited = unvisited - {pos}
    pos_score = VALVES[pos]["rate"] * moves
    # best move might be to do nothing
    # saving good spots for the second person
    best = travel(pos, 0, unvisited, part)
    for tunnel in unvisited:
        score = travel(tunnel, moves - 1 - ROUTES[(pos, tunnel)], unvisited, part)
        if score > best:
            best = score
    return best + pos_score


def part_one(moves):
    active = frozenset(v for v in VALVES if VALVES[v]["rate"] != 0)
    result = travel("AA", moves[0], active, moves[1:])
    print(f"part {moves}: {result}")


VALVES = parse(data_filename)
ROUTES = routes(VALVES)
part_one((30,))
part_one((26, 26))
