#!/usr/bin/python3
import math

data_filename = "2022-12-09.dat"
# data_filename = "test.dat"

DIRNS = {"R": (1, 0), "L": (-1, 0), "U": (0, 1), "D": (0, -1)}


def part_one(filename):
    h_pos = [0, 0]
    t_pos = [0, 0]

    locations = {tuple(t_pos)}
    with open(filename, "r") as f:
        for line in f:
            dirn, dist = line.strip().split()
            for _ in range(int(dist)):
                h_pos = [h_pos[0] + DIRNS[dirn][0], h_pos[1] + DIRNS[dirn][1]]

                if abs(h_pos[0] - t_pos[0]) + abs(h_pos[1] - t_pos[1]) > 2:
                    t_pos[0] += math.copysign(1, h_pos[0] - t_pos[0])
                    t_pos[1] += math.copysign(1, h_pos[1] - t_pos[1])
                elif abs(h_pos[0] - t_pos[0]) == 2:
                    t_pos[0] += math.copysign(1, h_pos[0] - t_pos[0])
                elif abs(h_pos[1] - t_pos[1]) == 2:
                    t_pos[1] += math.copysign(1, h_pos[1] - t_pos[1])

                locations.add(tuple(t_pos))

    print("two knot locations:", len(locations))


def part_two(filename, knots):
    pos = [[0, 0] for _ in range(knots)]
    locations = {tuple(pos[-1])}

    with open(filename, "r") as f:
        for line in f:
            dirn, dist = line.strip().split()

            for _ in range(int(dist)):
                # head motion
                pos[0] = (pos[0][0] + DIRNS[dirn][0], pos[0][1] + DIRNS[dirn][1])

                # tail motion: knot (n) follows knot (n-1)
                for num in range(1, knots):
                    if (
                        abs(pos[num - 1][0] - pos[num][0])
                        + abs(pos[num - 1][1] - pos[num][1])
                        > 2
                    ):
                        pos[num][0] += math.copysign(1, pos[num - 1][0] - pos[num][0])
                        pos[num][1] += math.copysign(1, pos[num - 1][1] - pos[num][1])
                    elif abs(pos[num - 1][0] - pos[num][0]) == 2:
                        pos[num][0] += math.copysign(1, pos[num - 1][0] - pos[num][0])
                    elif abs(pos[num - 1][1] - pos[num][1]) == 2:
                        pos[num][1] += math.copysign(1, pos[num - 1][1] - pos[num][1])

                # track the location of the last knot
                locations.add(tuple(pos[-1]))

    print(knots, "knot locations:", len(locations))


part_one(data_filename)
part_two(data_filename, 2)
part_two(data_filename, 10)
