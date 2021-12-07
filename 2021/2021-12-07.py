#!/usr/bin/python3

filename = "2021-12-07.dat"


def quad_cost(a, b):
    d = abs(a - b)
    return d * (d + 1) // 2


def part(fuel=0):
    with open(filename, "r") as f:
        for line in f.readlines():
            positions = [int(p) for p in line.strip().split(",")]

    cost = None
    for idx in range(max(positions) + 1):
        cost_fn = abs if fuel == 1 else quad_cost
        new_cost = sum(cost_fn(p, idx) for p in positions)
        if cost is None or new_cost < cost:
            cost = new_cost
            print("idx:", idx, "cost:", cost)


# part(fuel=1)
part(fuel=2)
