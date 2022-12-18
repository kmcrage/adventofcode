#!/usr/bin/python3
import itertools

data_filename = "2022-12-17.dat"
# data_filename = "test.dat"


def get_jet(filename):
    with open(filename, "r") as f:
        for line in f:
            return line.strip()


def get_rocks():
    data = """
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##

"""
    rocks = []
    rock = set()
    row = 0
    for line in data.split("\n"):
        line = line.strip()
        if line:
            for col, bit in enumerate(line):
                if bit == "#":
                    rock.add((row, col))
            row += 1
        elif rock:
            maxrow = max(i for i, _ in rock)
            maxcol = max(j for _, j in rock)
            rocks.append((rock, maxrow, maxcol))
            rock = set()
            row = 0
    if rock:
        rocks.append(rock)
    return rocks


def intersection(pos, rock, maxrow, chamber):
    return any((pos[0] + maxrow - i, pos[1] + j) in chamber for i, j in rock)


def get_hash(chamber, top, width):
    return "".join(
        "#" if (row, col) in chamber else "."
        for row, col in itertools.product(range(top, top - 7, -1), range(width + 1))
    )


def part_one(num_rocks, chamber, rocks, jet):
    width = max(j for _, j in chamber)
    time = 0
    repeats = {}
    heights = {}
    top = 0
    for n in range(num_rocks):
        rock, maxrow, maxcol = rocks[n % len(rocks)]
        pos = [max(i for i, _ in chamber) + 4, 2]

        while True:
            # update pos
            next_pos = pos[:]
            next_pos[1] = max(0, pos[1] + (1 if jet[time % len(jet)] == ">" else -1))
            if next_pos[1] + maxcol > width:
                next_pos[1] -= 1

            if intersection(next_pos, rock, maxrow, chamber):
                next_pos = pos

            time += 1
            if not intersection((next_pos[0] - 1, next_pos[1]), rock, maxrow, chamber):
                next_pos[0] -= 1
                pos = next_pos
                continue

            # update chamber, rock has stopped
            for i, j in rock:
                chamber.add((next_pos[0] + maxrow - i, next_pos[1] + j))

            # update top of chamber
            while any((top + 1, j) in chamber for j in range(width + 1)):
                top += 1

            # cache everything and look for cycles
            heights[n + 1] = top
            hashgrid = get_hash(chamber, top, width)
            idx = (hashgrid, n % len(rocks), time % len(jet))
            if idx in repeats:
                rocks_period = n - repeats[idx][0][0]
                height_period = top - repeats[idx][0][1]
                final_top = (
                    (num_rocks - repeats[idx][0][0]) // rocks_period
                ) * height_period + heights[
                    (num_rocks - repeats[idx][0][0]) % rocks_period + repeats[idx][0][0]
                ]
                print(f"part two height: {final_top}")
                return

            else:
                repeats[idx] = [(n, top)]
            break

    print("part one height:", top)


jet = get_jet(data_filename)
rocks = get_rocks()
chamber = {(0, j) for j in range(7)}
part_one(2022, chamber.copy(), rocks, jet)
part_one(1_000_000_000_000, chamber.copy(), rocks, jet)
