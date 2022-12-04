#!/usr/bin/python3

data_filename = "2022-12-04.dat"
# data_filename = "test.dat"


def part_one(filename):
    contained = 0
    with open(filename, "r") as f:
        for line in f:
            first, second = line.strip().split(',')
            first_range = [int(n) for n in first.split('-')]
            second_range = [int(n) for n in second.split('-')]
            if first_range[0] <= second_range[0]  and second_range[1] <= first_range[1]:
                contained += 1
            elif first_range[0] >= second_range[0]  and second_range[1] >= first_range[1]:
                contained += 1
    print("part one:", contained)


def part_two(filename):
    contained = 0
    with open(filename, "r") as f:
        for line in f:
            first, second = line.strip().split(',')
            first_range = [int(n) for n in first.split('-')]
            second_range = [int(n) for n in second.split('-')]
            if first_range[0] <= second_range[0] <= first_range[1]:
                contained += 1
            elif first_range[0] <= second_range[1] <= first_range[1]:
                contained += 1
            elif second_range[0] <=  first_range[0] <= second_range[1]:
                contained += 1
            elif second_range[0] <=  first_range[1] <= second_range[1]:
                contained += 1
    print("part two:", contained)



part_one(data_filename)
part_two(data_filename)
