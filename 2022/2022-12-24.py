#!/usr/bin/python3
import functools
import heapq
import math

data_filename = "2022-12-24.dat"
# data_filename = "test.dat"
# blizzards >V<^


def parse(filename):
    index = {b: i for i, b in enumerate(">v<^")}
    start = (0, 1)
    walls = set()
    blizzards = [set() for _ in range(4)]
    row = 0
    with open(filename, "r") as f:
        for line in f:
            for col, cell in enumerate(line.strip()):
                if cell == "E":
                    start = (row, col)
                elif cell not in ".#":
                    blizzards[index[cell]].add((row, col))
                elif cell == "#":
                    walls.add((row, col))
            row += 1
    return start, (frozenset(b) for b in blizzards), walls


@functools.lru_cache(maxsize=None)
def update_blizzard(blizzard, mv, size):
    new_blizzard = set()
    for i, j in blizzard:
        pos = [i + mv[0], j + mv[1]]
        if pos[0] == 0:
            pos[0] = size[0] - 1
        elif pos[0] == size[0]:
            pos[0] = 1
        if pos[1] == 0:
            pos[1] = size[1] - 1
        elif pos[1] == size[1]:
            pos[1] = 1
        new_blizzard.add(tuple(pos))
    return frozenset(new_blizzard)


def update_blizzards(blizzards, size):
    # blizzards >v<^
    dirns = ((0, 1), (1, 0), (0, -1), (-1, 0))
    new_blizzards = []
    for dirn, blizzard in zip(dirns, blizzards):
        blizzard = update_blizzard(blizzard, dirn, size)
        new_blizzards.append(blizzard)
    return new_blizzards


def visualise(blizzards, size):
    for i in range(size[0]):
        for j in range(size[1]):
            for blizzard, arrow in zip(blizzards, ">v<^"):
                if (i, j) in blizzard:
                    print(arrow, end="")
                    break
            else:
                print(".", end="")
        print("")
    print("")


def get_moves(pos, blizzards, walls, size):
    moves = []
    for offset in ((0, 1), (1, 0), (0, 0), (0, -1), (-1, 0)):
        move = (pos[0] + offset[0], pos[1] + offset[1])
        if move[0] < 0 or move[0] > size[0]:
            continue
        if move in walls:
            continue
        for blizzard in blizzards:
            if move in blizzard:
                break
        else:
            moves.append(move)
    return moves


def heuristic(pos, end, time):
    return time + abs(pos[0] - end[0]) + abs(pos[1] - end[1])


def part_one(start, end, blizzards, walls):
    best = math.inf
    best_blizzards = None
    size = (max(i for i, j in walls), max(j for i, j in walls))

    visited = set()
    queue = [[heuristic(start, end, 0), start, 0, blizzards]]
    last_est = 0
    while queue:
        est, pos, time, blizzards = heapq.heappop(queue)
        if est >= best:
            continue
        if est > last_est:
            # print("min:", est)
            last_est = est

        if (pos, time) in visited:
            continue
        visited.add((pos, time))
        # print(est, time, pos, best, queue[0] if len(queue) else "[]")
        # if (time, pos) in visited:
        #    continue
        blizzards = update_blizzards(blizzards, size)
        moves = get_moves(pos, blizzards, walls, size)
        time += 1
        # print(moves, time, len(moves))
        # visualise(blizzards, size)
        for move in moves:
            est = heuristic(move, end, time)
            if move == end:
                if time < best:
                    best = time
                    best_blizzards = blizzards
                    # print("best:", best)
            elif est < best:
                heapq.heappush(queue, [est, move, time, blizzards])
    print(f"fastest route: {best}")
    return best, best_blizzards


start, blizzards, walls = parse(data_filename)
end = (max(i for i, j in walls), max(j for i, j in walls) - 1)

total = 0
best, blizzards = part_one(start, end, blizzards, walls)
total += best
best, blizzards = part_one(end, start, blizzards, walls)
total += best
best, blizzards = part_one(start, end, blizzards, walls)
total += best
print(f"total time: {total}")
