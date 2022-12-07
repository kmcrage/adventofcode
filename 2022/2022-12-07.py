#!/usr/bin/python3

data_filename = "2022-12-07.dat"
# data_filename = "test.dat"


def part_one(filename, max_size):
    filesystem = {}
    cwd = None
    with open(filename, "r") as f:
        # parse
        for line in f:
            # print(line)
            tokens = line.split()
            if tokens[1] == "cd":
                if tokens[2] == "/":
                    cwd = "/"
                elif tokens[2] == "..":
                    cwd = "/".join(cwd.split("/")[:-1])
                else:
                    cwd += f"/{tokens[2]}"
                if cwd not in filesystem:
                    filesystem[cwd] = 0

            elif tokens[0] not in ("$", "dir"):
                dr = cwd
                while dr:
                    filesystem[dr] += int(tokens[0])
                    dr = "/".join(dr.split("/")[:-1])

            # print("cwd:", cwd)
    result = sum(s for s in filesystem.values() if s <= max_size)
    print(f"part one: {result}")


def part_two(filename, total, target):
    filesystem = {}
    cwd = None
    with open(filename, "r") as f:
        # parse
        for line in f:
            # print(line)
            tokens = line.split()
            if tokens[1] == "cd":
                if tokens[2] == "/":
                    cwd = "/"
                elif tokens[2] == "..":
                    cwd = "/".join(cwd.split("/")[:-1])
                else:
                    cwd += f"/{tokens[2]}"
                if cwd not in filesystem:
                    filesystem[cwd] = 0

            elif tokens[0] not in ("$", "dir"):
                dr = cwd
                while dr:
                    filesystem[dr] += int(tokens[0])
                    dr = "/".join(dr.split("/")[:-1])

            # print("cwd:", cwd)
    print(f'filesystem: {filesystem["/"]}')
    removal = target + filesystem["/"] - total
    print(f"remove: {removal}")
    min_removal = total
    for s in filesystem.values():
        if s < min_removal and s >= removal:
            min_removal = s
    print(f"part two: {min_removal}")


part_one(data_filename, 100_000)
part_two(data_filename, 70_000_000, 30_000_000)
