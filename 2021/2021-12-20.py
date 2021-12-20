#!/usr/bin/python3

filename = "2021-12-20.dat"


def get_neighbours(pixels):
    nhbrs = set()
    for pixel in pixels:
        for i in [-1, 0, 1]:
            for j in [-1, 0, 1]:
                nhbrs.add(tuple([pixel[0] + i, pixel[1] + j]))
    return nhbrs


def take_step(pixels, default, algorithm):
    updated = set()
    updated_default = algorithm[0] if default == "." else algorithm[511]
    binary = ("1", "0") if default == "." else ("0", "1")
    for nhbr in get_neighbours(pixels):
        code = ""
        for i in [-1, 0, 1]:
            for j in [-1, 0, 1]:
                code += binary[0] if (nhbr[0] + i, nhbr[1] + j) in pixels else binary[1]

        idx = int(code, 2)
        if algorithm[idx] != updated_default:
            updated.add(nhbr)
    return updated, updated_default


def take_steps(pixels, algorithm, steps):
    print("init", "num lights", len(pixels))
    default = "."
    for step in range(steps):
        pixels, default = take_step(pixels, default, algorithm)
        print(
            "step", step, "num lights" if default == "." else "num darks", len(pixels)
        )
    return pixels


algorithm = ""
i = 0
pixels = set()
with open(filename, "r") as f:
    mode = "algorithm"
    for line in f.readlines():
        line = line.strip()

        if line == "":
            mode = "input"
            continue

        if mode == "algorithm":
            algorithm += line
            continue

        for j, pixel in enumerate(line):
            if pixel == "#":
                pixels.add(tuple([i, j]))
        i += 1

take_steps(pixels, algorithm, 50)
