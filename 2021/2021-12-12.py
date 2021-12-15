#!/usr/bin/python3

from collections import Counter

filename = "2021-12-12.dat"


def get_map():
    caves = {}
    with open(filename, "r") as f:
        for line in f.readlines():
            tunnel = line.strip().split("-")
            for i in [0, 1]:
                if tunnel[i] not in caves:
                    caves[tunnel[i]] = []
                caves[tunnel[i]].append(tunnel[1 - i])
    return caves


def move(path, caves, part=1):
    routes = 0
    counter = Counter(path)
    repeat_small = False
    for cave in counter:
        if counter[cave] == 2 and cave == cave.lower():
            repeat_small = True

    for dst in caves[path[-1]]:
        # only visit the end once
        if dst == "end":
            routes += 1
            continue

        # dont revisit the start
        if dst == "start":
            continue

        if (
            dst == dst.lower()
            and dst in counter
            and (part == 1 or repeat_small)
        ):
            continue

        routes += move(path + [dst], caves, part=part)
    return routes


def part_one():
    caves = get_map()
    routes = move(["start"], caves, part=1)
    print("routes one:", routes)


def part_two():
    caves = get_map()
    routes = move(["start"], caves, part=2)
    print("routes two:", routes)


part_one()
part_two()
