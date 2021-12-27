#!/usr/bin/python3
# see q15

import heapq


def move_cost(mv, cave, cave_pos, node):
    costs = {"A": 1, "B": 10, "C": 100, "D": 1000}
    return (abs(mv) + len(cave) - cave_pos) * costs[node]


def move_to_room(n, node, state, step, nhbrs):
    for dst in range(n + step, len(state) if step == 1 else -1, step):
        if len(state[dst]) == 1:
            if state[dst] != ".":
                break
            continue
        if any(state[dst][c] not in set([".", node]) for c in range(len(state[dst]))):
            continue
        for r in range(len(state[dst]) - 1, 0, -1):
            if state[dst][r] != ".":
                break
            nhbr = list(state)
            nhbr[dst] = state[dst][:r] + node
            if r < len(state[dst]) - 1:
                nhbr[dst] += state[dst][r + 1 :]
            nhbr[n] = "."
            nhbrs.append((tuple(nhbr), move_cost(n - dst, nhbr[dst], r, node)))


def move_from_room(n, node, src, state, step, nhbrs):
    for dst in range(n + step, len(state) if step == 1 else -1, step):
        if len(state[dst]) > 1:
            continue
        if state[dst] != ".":
            break
        nhbr = list(state)
        nhbr[dst] = node[src]
        nhbr[n] = node[:src] + "." + node[src + 1 :]
        nhbrs.append((tuple(nhbr), move_cost(n - dst, node, src, node[src])))


def get_moves(state):
    # example state: (".", ".", "AAB", ".", "BDC", ".", "CCB", ".", "DAD", ".", ".")

    nhbrs = []
    for n, node in enumerate(state):
        if len(node) > 1:
            # this is a room
            src = len(node) - 1
            while node[src] == ".":
                src -= 1
            if src == 0 or all(node[i] == node[0] for i in range(1, src + 1)):
                continue
            # node[src] is the src, what are the dst?
            move_from_room(n, node, src, state, 1, nhbrs)
            move_from_room(n, node, src, state, -1, nhbrs)

        elif node != ".":
            # corridor: move to room
            move_to_room(n, node, state, 1, nhbrs)
            move_to_room(n, node, state, -1, nhbrs)

    return nhbrs


def dijkstra(start, end):
    visited = set()
    costs = {start: 0}
    # candidates = [(cost, state), ...] which we keep sorted
    candidates = [(0, start)]
    while end not in visited:
        state = heapq.heappop(candidates)[1]
        if state in visited:
            continue

        for nghbr, move_cost in get_moves(state):
            if nghbr in visited:
                continue
            cost = costs[state] + move_cost
            if nghbr not in costs or cost < costs[nghbr]:
                costs[nghbr] = cost
                heapq.heappush(candidates, (cost, nghbr))

        visited.add(state)

    print("result:", costs[end], "(visited:", len(visited), ")")


# test
# start = (".", ".", "AAB", ".", "BDC", ".", "CCB", ".", "DAD", ".", ".")

# part one:
print("part one")
start = (".", ".", "ACD", ".", "BAB", ".", "CDC", ".", "DBA", ".", ".")
end = (".", ".", "AAA", ".", "BBB", ".", "CCC", ".", "DDD", ".", ".")
dijkstra(start, end)

# part two
print("part two")
start = (".", ".", "ACDDD", ".", "BABCB", ".", "CDABC", ".", "DBCAA", ".", ".")
end = (".", ".", "AAAAA", ".", "BBBBB", ".", "CCCCC", ".", "DDDDD", ".", ".")
dijkstra(start, end)
