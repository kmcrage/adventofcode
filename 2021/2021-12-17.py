#!/usr/bin/python3
# note that the trajectory hits y = 0 if v > 0


def trajectory(velocity, target):
    x = 0
    y = 0
    u = velocity[0]
    v = velocity[1]
    y_max = 0

    while x <= target[0][1] and y >= target[1][0]:
        x += u
        y += v
        if u:
            u -= u // abs(u)
        v -= 1

        if y > y_max:
            y_max = y

        if target[0][0] <= x <= target[0][1] and target[1][0] <= y <= target[1][1]:
            # hit target
            return y_max

    return -1  # missed


def part_one(xmin, xmax, ymin, ymax):
    height = 0
    hits = 0
    for u in range(xmax + 2):
        for v in range(ymin - 1, abs(ymin) + 2):
            y_max = trajectory((u, v), ((xmin, xmax), (ymin, ymax)))
            if y_max > height:
                height = y_max
            if y_max > -1:
                hits += 1
    print("max height", height)
    print("hits", hits)


# part_one(20, 30, -10, -5)
part_one(94, 151, -156, -103)
