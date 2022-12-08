#!/usr/bin/python3

data_filename = "2022-12-07.dat"
# data_filename = "test.dat"


def parser(filename):
    filesystem = {}
    with open(filename, "r") as f:
        cwd = None
        for line in f:
            # print(line)
            tokens = line.split()

            # navigate
            if tokens[1] == "cd":
                if tokens[2] == "/":
                    cwd = ['']
                elif tokens[2] == "..":
                    cwd.pop()
                else:
                    cwd.append(tokens[2])

            # file sizes
            elif tokens[0] not in ("$", "dir"):
                dr = cwd.copy()
                while dr:
                    path = "/".join(dr)
                    if path not in filesystem:
                        filesystem[path] = 0
                    filesystem[path] += int(tokens[0])

                    dr.pop()
    return filesystem


def part_one(filesystem, max_size):
    result = sum(s for s in filesystem.values() if s <= max_size)
    print(f"part one, small directories: {result}")


def part_two(filesystem, total, target):
    removal = target + filesystem[""] - total
    min_removal = min(s for s in filesystem.values() if s >= removal)
    print(f"part two, minimal removal: {min_removal}")


filesystem = parser(data_filename)
part_one(filesystem, 100_000)
part_two(filesystem, 70_000_000, 30_000_000)
