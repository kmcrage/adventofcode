#!/usr/bin/python3

filename = "2021-12-06.dat"


def part_one(days=18):
    state = [0] * 9
    with open(filename, "r") as f:
        for line in f.readlines():
            ages = line.strip().split(",")
            for age in ages:
                state[int(age)] += 1

    for day in range(days):
        future = [0] * 9
        future[8] += state[0]
        future[6] += state[0]
        for i in range(1, 9):
            future[i - 1] += state[i]
        state = future
        print(day, state, sum(state))


# part_one(days=80)
part_one(days=256)
