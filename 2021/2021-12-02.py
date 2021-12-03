#!/usr/bin/python3


filename = "2021-12-02.dat"


def part_one():
    # forward, depth
    pos = [0, 0]
    with open(filename, "r") as f:
        for line in f.readlines():
            move = line.strip().split()
            if move[0] == "forward":
                pos[0] += int(move[1])
            elif move[0] == "up":
                pos[1] -= int(move[1])
            elif move[0] == "down":
                pos[1] += int(move[1])
            else:
                print("error:", move)
    print(pos[0] * pos[1])


def part_two():
    # forward, depth, aim
    pos = [0, 0, 0]
    with open(filename, "r") as f:
        for line in f.readlines():
            move = line.strip().split()
            if move[0] == "forward":
                pos[0] += int(move[1])
                pos[1] += int(move[1]) * pos[2]
            elif move[0] == "up":
                pos[2] -= int(move[1])
            elif move[0] == "down":
                pos[2] += int(move[1])
            else:
                print("error:", move)
    print(pos[0] * pos[1])


part_two()
