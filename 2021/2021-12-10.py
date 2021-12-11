#!/usr/bin/python3

import re

filename = "2021-12-10.dat"
part_one_scores = {")": 3, "]": 57, "}": 1197, ">": 25137}
part_two_scores = {"(": 1, "[": 2, "{": 3, "<": 4}


def part_one():
    part_one_score = 0
    scores = []
    with open(filename, "r") as f:
        for line in f.readlines():
            line = line.strip()
            last = ""
            while last != line:
                last = line
                line = re.sub(r"(\[\]|\(\)|\{\}|<>)", "", line)

            match = re.search(r"(\)|}|\]|>)", line)
            if match:
                part_one_score += part_one_scores[match.group(0)]
            else:
                tokens = list(line)
                score = 0
                while tokens:
                    score = score * 5 + part_two_scores[tokens.pop()]
                scores.append(score)

    scores.sort()
    print("part one score:", part_one_score)
    print("part two score:", scores[len(scores) // 2])


part_one()
