#!/usr/bin/python3

import copy
import re

filename = "2021-12-18.dat"


def parser(line):
    snailfish = []
    depth = 0
    idx = 0
    while idx < len(line):
        if line[idx : idx + 1] == "[":
            depth += 1
            idx += 1
            continue
        elif line[idx : idx + 1] == "]":
            depth -= 1
            idx += 1
            continue

        m = re.match(r"\d+", line[idx:])
        if m:
            snailfish.append([int(m.group(0)), depth])
            idx += len(m.group(0))
            continue

        idx += 1
    return snailfish


def snailfish_add(sf_one, sf_two):
    offset = 1 if sf_one and sf_two else 0
    return copy.deepcopy([[p[0], p[1] + offset] for p in sf_one + sf_two])


def explode(snailfish):
    for idx, val in enumerate(snailfish):
        # explode
        if val[1] > 4 and snailfish[idx][1] == snailfish[idx + 1][1]:
            if idx > 0:
                snailfish[idx - 1][0] += snailfish[idx][0]
            if idx + 2 < len(snailfish):
                snailfish[idx + 2][0] += snailfish[idx + 1][0]
            snailfish = (
                snailfish[:idx] + [[0, snailfish[idx][1] - 1]] + snailfish[idx + 2 :]
            )
            break
    return snailfish


def split_ten(snailfish):
    for idx, val in enumerate(snailfish):
        if val[0] > 9:
            snailfish = (
                snailfish[:idx]
                + [[val[0] // 2, val[1] + 1], [(val[0] + 1) // 2, val[1] + 1]]
                + snailfish[idx + 1 :]
            )
            break
    return snailfish


def magnitude(snailfish):
    while len(snailfish) > 1:
        for idx, val in enumerate(snailfish):
            if snailfish[idx][1] == snailfish[idx + 1][1]:
                snailfish = (
                    snailfish[:idx]
                    + [
                        [
                            3 * snailfish[idx][0] + 2 * snailfish[idx + 1][0],
                            snailfish[idx][1] - 1,
                        ]
                    ]
                    + snailfish[idx + 2 :]
                )
                break
    return snailfish[0][0]


def part_one():
    snailfish = []
    with open(filename, "r") as f:
        for line in f.readlines():
            current = parser(line.strip())

            print("add", current)
            snailfish = snailfish_add(snailfish, current)

            prev_len = 0
            while prev_len != len(snailfish):
                prev_len = len(snailfish)

                snailfish = explode(snailfish)
                if prev_len != len(snailfish):
                    continue
                snailfish = split_ten(snailfish)

            print("reduced", snailfish)

    print("magnitude:", magnitude(snailfish))


def part_two():
    snailfishs = []
    with open(filename, "r") as f:
        for line in f.readlines():
            snailfishs.append(parser(line.strip()))

    mag = 0
    for one in snailfishs:
        for two in snailfishs:
            if one == two:
                continue
            snailfish = snailfish_add(one, two)

            # reduce
            prev_len = 0
            while prev_len != len(snailfish):
                prev_len = len(snailfish)

                snailfish = explode(snailfish)
                if prev_len != len(snailfish):
                    continue
                snailfish = split_ten(snailfish)

            mag = max(mag, magnitude(snailfish))
    print("max mag", mag)


# part_one()
part_two()
