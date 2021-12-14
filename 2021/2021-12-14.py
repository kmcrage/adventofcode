#!/usr/bin/python3

from collections import Counter

filename = "2021-12-14.dat"


def parser():
    start = ""
    rules = {}
    with open(filename, "r") as f:
        for i, line in enumerate(f.readlines()):
            line = line.strip()
            if not start:
                start = line
                continue

            if "->" in line:
                rule = line.split()
                rules[rule[0]] = rule[-1]
    return start, rules


def part_one(start, rules, steps):
    polymer = start
    print("template:", polymer)
    for step in range(steps):
        new_polymer = "".join(
            polymer[i] + rules[polymer[i : i + 2]] for i in range(len(polymer) - 1)
        )
        polymer = new_polymer + polymer[-1]
        print("step:", step, "length:", len(polymer))

    cnt = Counter(polymer).most_common()
    print("result after", steps, "steps:", cnt[0][1] - cnt[-1][1])


def part_two(start, rules, steps):
    # initialise pair dictionary
    polymer = {}
    print("template:", start)
    for i in range(len(start) - 1):
        pair = start[i : i + 2]
        polymer[pair] = polymer.get(pair, 0) + 1

    # evolve pairs
    for step in range(steps):
        new_polymer = {}
        for pair, num in polymer.items():
            for new_pair in [pair[0] + rules[pair], rules[pair] + pair[1]]:
                new_polymer[new_pair] = new_polymer.get(new_pair, 0) + num
        polymer = new_polymer
        print("step:", step, "length:", sum(polymer.values()) + 1)

    # score using the first of each pair, plus the last symbol of the polymer
    score = {start[-1]: 1}
    for pair, num in polymer.items():
        score[pair[0]] = score.get(pair[0], 0) + num
    print("score after", steps, "steps:", max(score.values()) - min(score.values()))


start, rules = parser()
part_one(start, rules, 10)
print()
part_two(start, rules, 40)
