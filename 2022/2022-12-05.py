#!/usr/bin/python3

data_filename = "2022-12-05.dat"
# data_filename = "test.dat"


def part_one(filename):
    crates = []
    with open(filename, "r") as f:
        # parse
        for line in f:
            line = line.rstrip()
            if not line:
                break

            idx = 0
            pos = 1
            while pos < len(line):
                if idx >= len(crates):
                    crates.append([])
                if line[pos] != " ":
                    crates[idx].insert(0, line[pos])
                idx += 1
                pos += 4

        # move
        for line in f:
            tokens = line.split()
            num = int(tokens[1])
            frm = int(tokens[3]) - 1
            to = int(tokens[5]) - 1
            for _ in range(num):
                crates[to].append(crates[frm].pop(-1))

    result = "".join([c[-1] for c in crates])
    print("part one:", result)


def part_two(filename):
    crates = []
    with open(filename, "r") as f:
        # parse
        for line in f:
            line = line.rstrip()
            if not line:
                break

            idx = 0
            pos = 1
            while pos < len(line):
                if idx >= len(crates):
                    crates.append([])
                if line[pos] != " ":
                    crates[idx].insert(0, line[pos])
                idx += 1
                pos += 4

        # move
        for line in f:
            tokens = line.split()
            num = int(tokens[1])
            frm = int(tokens[3]) - 1
            to = int(tokens[5]) - 1

            crates[to] = crates[to] + crates[frm][-num:]
            crates[frm] = crates[frm][:-num]

    result = "".join([c[-1] for c in crates])
    print("part two:", result)


part_one(data_filename)
part_two(data_filename)
