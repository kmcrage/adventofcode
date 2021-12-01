#!/usr/bin/python3


filename = "2021-12-01.dat"


def part_one():
    increments = 0
    last = 9999999
    with open(filename, "r") as f:
        for line in f.readlines():
            line = line.strip()
            depth = int(line)
            if depth > last:
                increments += 1
            last = depth
    print(increments)


def part_two(length=3):
    increments = 0
    last = []
    with open(filename, "r") as f:
        for line in f.readlines():
            line = line.strip()
            depth = int(line)
            last.append(depth)
            if len(last) > length and last.pop(0) < depth:
                increments += 1
    print(increments)


part_two()
