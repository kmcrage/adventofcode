#!/usr/bin/python3

data_filename = "2022-12-02.dat"
# data_filename = "test.dat"


def part_one(filename):
    game = {
        "X": {"result": {"A": 3, "B": 0, "C": 6}, "score": 1},
        "Y": {"result": {"A": 6, "B": 3, "C": 0}, "score": 2},
        "Z": {"result": {"A": 0, "B": 6, "C": 3}, "score": 3},
    }

    score = 0
    with open(filename, "r") as f:
        for line in f.readlines():
            tokens = line.split()
            score += game[tokens[1]]["score"]
            score += game[tokens[1]]["result"][tokens[0]]
    print("part one score:", score)


def part_two(filename):
    game = {
        "X": {"score": {"A": 3, "B": 1, "C": 2}, "result": 0},
        "Y": {"score": {"A": 1, "B": 2, "C": 3}, "result": 3},
        "Z": {"score": {"A": 2, "B": 3, "C": 1}, "result": 6},
    }

    score = 0
    with open(filename, "r") as f:
        for line in f.readlines():
            tokens = line.split()
            score += game[tokens[1]]["result"]
            score += game[tokens[1]]["score"][tokens[0]]
    print("part two score:", score)


part_one(data_filename)
part_two(data_filename)
