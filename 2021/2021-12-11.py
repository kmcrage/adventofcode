#!/usr/bin/python3


filename = "2021-12-11.dat"


def neighbours(pos):
    for i in range(-1, 2):
        for j in range(-1, 2):
            if i or j:
                yield (pos[0] + i, pos[1] + j)


def flash(pos, octopus):
    flashes = 1
    octopus[pos] = 0
    for neighbour in neighbours(pos):
        if neighbour not in octopus:
            continue
        if octopus[neighbour] > 0:
            octopus[neighbour] += 1
        if octopus[neighbour] > 9:
            flashes += flash(neighbour, octopus)
    return flashes


def pprint(octopus):
    for i in range(10):
        for j in range(10):
            print(octopus[(i, j)], end="")
        print()
    print()


def part_one(steps):
    octopus = {}
    with open(filename, "r") as f:
        for i, line in enumerate(f.readlines()):
            for j, state in enumerate(line.strip()):
                octopus[(i, j)] = int(state)

    pprint(octopus)

    flashes = 0
    for step in range(steps):
        # increase by one
        for pos in octopus:
            octopus[pos] += 1

        # then flash
        for pos in octopus:
            if octopus[pos] > 9:
                flashes += flash(pos, octopus)

        pprint(octopus)
        print("step:", step, "flashes:", flashes)
        print()


def part_two():
    octopus = {}
    with open(filename, "r") as f:
        for i, line in enumerate(f.readlines()):
            for j, state in enumerate(line.strip()):
                octopus[(i, j)] = int(state)

    pprint(octopus)

    step = 0
    acc = 1
    while acc:
        step += 1
        # increase by one
        for pos in octopus:
            octopus[pos] += 1

        # then flash
        for pos in octopus:
            if octopus[pos] > 9:
                flash(pos, octopus)

        acc = sum(o for o in octopus.values())
    print("sync step", step)


part_one(100)
part_two()
