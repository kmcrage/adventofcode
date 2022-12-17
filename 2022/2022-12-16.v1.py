#!/usr/bin/python3
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


@functools.lru_cache(maxsize=None)
def travel(pos, moves, unvisited, part):
    moves -= 1
    if moves <= 0:
        return travel("AA", part[0], unvisited, part[1:]) if part else 0

    pos_unvisited = unvisited - {pos}
    valve_off = pos in unvisited
    best = 0
    for tunnel in valves[pos]["tunnels"]:
        if valve_off:
            score = valves[pos]["rate"] * moves
            score += travel(tunnel, moves - 1, pos_unvisited, part)
            if score > best:
                best = score

        score = travel(tunnel, moves, unvisited, part)
        if score > best:
            best = score

    return best


def part_one(moves):
    active_valves = frozenset(v for v in valves if valves[v]["rate"] != 0)
    result = travel("AA", moves[0], active_valves, moves[1:])
    print(f"part {moves}: {result}")


valves = parse(data_filename)
part_one((30,))
part_one((26, 26))
