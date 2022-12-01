#!/usr/bin/python3
import bisect

DATA = "2022-12-01.dat"
# DATA = "test.dat"


def elf_order():
    elves = []
    elf_total = 0
    with open(DATA, "r") as f:
        for line in f.readlines():
            line = line.strip()

            if line == "":
                bisect.insort(elves, elf_total)
                elf_total = 0
                continue

            elf_total += int(line)

    bisect.insort(elves, elf_total)
    return elves


sorted_elves = elf_order()

print("part one, max:", sorted_elves[-1])
print("part two, sum:", sum(sorted_elves[-3:]))
