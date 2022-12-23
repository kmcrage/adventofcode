#!/usr/bin/python3
import itertools
import math

data_filename = "2022-12-23.dat"
# data_filename = "test.dat"

candidates = [
    ((-1, 0), [(-1, -1), (-1, 0), (-1, 1)]),  # N
    ((1, 0), [(1, -1), (1, 0), (1, 1)]),  # S
    ((0, -1), [(1, -1), (0, -1), (-1, -1)]),  # W
    ((0, 1), [(1, 1), (0, 1), (-1, 1)]),  # E
]


def parse(filename):
    plan = set()
    row = 0
    with open(filename, "r") as f:
        for line in f:
            for col, elf in enumerate(line.strip()):
                if elf == "#":
                    plan.add((row, col))
            row += 1
    return plan


def result(plan):
    rmin = math.inf
    rmax = -math.inf
    cmin = math.inf
    cmax = -math.inf
    for r, c in plan:
        rmin = min(rmin, r)
        rmax = max(rmax, r)
        cmin = min(cmin, c)
        cmax = max(cmax, c)

    if (rmax - rmin) < 20:
        for r in range(rmin, rmax + 1):
            for c in range(cmin, cmax + 1):
                print("#" if (r, c) in plan else ".", end="")
            print("")

    print(f"empty ground tiles: {(rmax-rmin+1)*(cmax-cmin+1)-len(plan)}")


def step(plan, first_candidate, candidates):
    moves = {}
    for i, j in plan:
        for dx, dy in itertools.product([-1, 0, 1], repeat=2):
            if dx == dy == 0:
                continue
            if (i + dx, j + dy) in plan:
                break
        else:
            continue

        for c in range(4):
            mv, tests = candidates[(c + first_candidate) % 4]
            for offset in tests:
                pt = (i + offset[0], j + offset[1])
                if pt in plan:
                    break
            else:
                future = (i + mv[0], j + mv[1])
                if future in moves:
                    moves[future].append((i, j))
                else:
                    moves[future] = [(i, j)]
                break

    cnt = 0
    for new_pt, old_pts in moves.items():
        if len(old_pts) == 1:
            plan.remove(old_pts[0])
            plan.add(new_pt)
            cnt += 1

    return not bool(cnt)


def part_one(plan, candidates, steps):
    first_candidate = 0
    for _ in range(steps):
        step(plan, first_candidate, candidates)
        first_candidate = (first_candidate + 1) % 4
    return plan


def part_two(plan, candidates):
    first_candidate = 0
    rounds = 1
    while True:
        if step(plan, first_candidate, candidates):
            print(f"first idle round: {rounds}")
            break

        first_candidate = (first_candidate + 1) % 4
        rounds += 1
    return plan


plan = parse(data_filename)
result(part_one(plan.copy(), candidates.copy(), 10))
part_two(plan.copy(), candidates.copy())
