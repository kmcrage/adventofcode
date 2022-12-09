#!/usr/bin/python3

data_filename = "2022-12-09.dat"
# data_filename = "test.dat"

DIRNS = {"R": (1, 0), "L": (-1, 0), "U": (0, 1), "D": (0, -1)}


def sign(num):
    return (num > 0) - (num < 0)


def move_tail(tail, head):
    if abs(head[0] - tail[0]) + abs(head[1] - tail[1]) > 2:
        tail[0] += sign(head[0] - tail[0])
        tail[1] += sign(head[1] - tail[1])
    elif abs(head[0] - tail[0]) == 2:
        tail[0] += sign(head[0] - tail[0])
    elif abs(head[1] - tail[1]) == 2:
        tail[1] += sign(head[1] - tail[1])


def part_one(filename):
    h_pos = [0, 0]
    t_pos = [0, 0]

    locations = {tuple(t_pos)}
    with open(filename, "r") as f:
        for line in f:
            dirn, dist = line.strip().split()
            for _ in range(int(dist)):
                h_pos = [h_pos[0] + DIRNS[dirn][0], h_pos[1] + DIRNS[dirn][1]]
                move_tail(t_pos, h_pos)
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
                    move_tail(pos[num], pos[num - 1])
                # track the location of the last knot
                locations.add(tuple(pos[-1]))

    print(knots, "knot locations:", len(locations))


part_one(data_filename)
part_two(data_filename, 2)
part_two(data_filename, 10)
