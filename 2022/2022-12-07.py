#!/usr/bin/python3

data_filename = "2022-12-07.dat"
# data_filename = "test.dat"

def cdup(cwd):
    return "/".join(cwd.split("/")[:-1])


def parser(filename):
    filesystem = {}
    cwd = None
    with open(filename, "r") as f:
        for line in f:
            # print(line)
            tokens = line.split()

            # navigate
            if tokens[1] == "cd":
                if tokens[2] == "/":
                    cwd = "/"
                elif tokens[2] == "..":
                    cwd = cdup(cwd)
                else:
                    cwd += f"/{tokens[2]}"

                if cwd not in filesystem:
                    filesystem[cwd] = 0

            # file sizes
            elif tokens[0] not in ("$", "dir"):
                dr = cwd
                while dr:
                    filesystem[dr] += int(tokens[0])
                    dr = cdup(dr)

    return filesystem


def part_one(filesystem, max_size):
    result = sum(s for s in filesystem.values() if s <= max_size)
    print(f"part one, small directories: {result}")


def part_two(filesystem, total, target):
    removal = target + filesystem["/"] - total
    # print(f"remove: {removal}")
    min_removal = total
    for s in filesystem.values():
        if s < min_removal and s >= removal:
            min_removal = s
    print(f"part two, minimal removal: {min_removal}")


filesystem = parser(data_filename)
part_one(filesystem, 100_000)
part_two(filesystem, 70_000_000, 30_000_000)
