#!/usr/bin/python3

data_filename = "2022-12-10.dat"
# data_filename = "test.dat"


def part_one(filename):
    result = 0
    cycles = 0
    register = 1
    with open(filename, "r") as f:
        for line in f:
            tokens = line.split()
            if tokens[0] == "noop":
                cycles += 1
                if (cycles - 20) % 40 == 0:
                    result += cycles * register
                    print(cycles, cycles * register)
            else:
                cycles += 2
                if (cycles - 20) % 40 == 0:
                    result += cycles * register
                    print(cycles, cycles * register)
                elif (cycles - 20) % 40 == 1:
                    result += (cycles - 1) * register
                    print(cycles - 1, (cycles - 1) * register)
                register += int(tokens[1])

    print(f"result one: {result}")


def process(cycles, register):
    if cycles % 40 == 0:
        print("")
    if abs((cycles % 40) - register) < 2:
        print("#", end="")
    else:
        print(".", end="")


def part_two(filename):
    cycles = 0
    register = 1
    with open(filename, "r") as f:
        for line in f:
            tokens = line.split()
            if tokens[0] == "noop":
                process(cycles, register)
                cycles += 1
            else:
                for _ in range(2):
                    process(cycles, register)
                    cycles += 1
                register += int(tokens[1])
    print("")


part_one(data_filename)
part_two(data_filename)
