#!/usr/bin/python3

data_filename = "2022-12-03.dat"
# data_filename = "test.dat"


def part_one(filename):
    score = 0
    with open(filename, "r") as f:
        for line in f:
            first = set(line[: len(line) // 2])
            second = set(line[len(line) // 2 :])
            c = first.intersection(second).pop()
            c_score = ord(c) - 96
            if c_score <= 0:
                c_score += 58
            # print(c, c_score)
            score += c_score
    print("part one sum:", score)


def part_two(filename):
    score = 0
    with open(filename, "r") as f:
        group = []
        for line in f:
            group.append(line.strip())
            if len(group) < 3:
                continue

            result = set(group.pop())
            for line in group:
                result = result.intersection(set(line))
                # print("result:", result)

            c = result.pop()
            c_score = ord(c) - 96
            if c_score <= 0:
                c_score += 58
            # print(c, c_score)
            score += c_score
            group = []
    print("part two sum:", score)


part_one(data_filename)
part_two(data_filename)
