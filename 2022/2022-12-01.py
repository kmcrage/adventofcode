#!/usr/bin/python3
import bisect

filename = "2022-12-01.dat"
# filename = "test.dat"


def elf_order(filename):
    elves = []
    elf_total = 0
    with open(filename, "r") as f:
        for line in f.readlines():
            line = line.strip()

            if line == "":
                bisect.insort(elves, elf_total)
                elf_total = 0
                continue

            elf_total += int(line)

    bisect.insort(elves, elf_total)
    return elves


sorted_elves = elf_order(filename)

print("part one, max:", sorted_elves[-1])
print("part two, sum:", sum(sorted_elves[-3:]))
