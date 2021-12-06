#!/usr/bin/python3


filename = "2021-12-05.dat"


def part_two():
    grid = {}
    with open(filename, "r") as f:
        for line in f.readlines():
            tokens = line.strip().split()
            src = [int(n) for n in tokens[0].split(",")]
            dst = [int(n) for n in tokens[2].split(",")]

            dir = [dst[0] - src[0], dst[1] - src[1]]

            #
            # This test for part one
            #
            # if dir[0] != 0 and dir[1] != 0:
            #    continue

            if dir[0] != 0:
                dir[0] //= abs(dir[0])
            if dir[1] != 0:
                dir[1] //= abs(dir[1])

            pos = (src[0] - dir[0], src[1] - dir[1])
            while pos[0] != dst[0] or pos[1] != dst[1]:
                pos = (pos[0] + dir[0], pos[1] + dir[1])
                if pos in grid:
                    grid[pos] += 1
                else:
                    grid[pos] = 1

    cnt = sum(score > 1 for score in grid.values())
    print("count", cnt)


part_two()
