#!/usr/bin/python3
import re

data_filename = "2022-12-15.dat"
# data_filename = "test.dat"


def part_one(filename, row):
    scan_row = {}
    with open(filename, "r") as f:
        for line in f:
            m = re.search(r"x=(-?\d+), y=(-?\d+).*x=(-?\d+), y=(-?\d+)", line)
            if not m:
                continue
            sensor = int(m[1]), int(m[2])
            beacon = int(m[3]), int(m[4])
            if sensor[1] == row:
                scan_row[sensor[0]] = "S"
            if beacon[1] == row:
                scan_row[beacon[0]] = "B"
            m_dist = abs(sensor[0] - beacon[0]) + abs(sensor[1] - beacon[1])
            h = abs(row - sensor[1])
            for x in range(sensor[0] - m_dist + h, sensor[0] + m_dist - h + 1):
                if x not in scan_row:
                    scan_row[x] = "#"
    result = len([s for s in scan_row if scan_row[s] == "#"])
    print(f"positions {result}")


def circle(disk, maxsize):
    centre, radius = disk
    for x in range(radius + 2):
        offset = (x, radius + 1 - x)
        for p in (
            (centre[0] + offset[0], centre[1] + offset[1]),
            (centre[0] + offset[0], centre[1] - offset[1]),
            (centre[0] - offset[0], centre[1] + offset[1]),
            (centre[0] - offset[0], centre[1] - offset[1]),
        ):
            if 0 <= p[0] <= maxsize and 0 <= p[1] <= maxsize:
                yield p


def detected(pos, disk):
    centre, radius = disk
    x_dist = abs(centre[0] - pos[0])
    return False if x_dist > radius else x_dist + abs(centre[1] - pos[1]) <= radius


def parser(filename):
    disks = []
    print("parse")
    with open(filename, "r") as f:
        for line in f:
            m = re.search(r"x=(-?\d+), y=(-?\d+).*x=(-?\d+), y=(-?\d+)", line)
            if not m:
                continue
            sensor = int(m[1]), int(m[2])
            beacon = int(m[3]), int(m[4])
            m_dist = abs(sensor[0] - beacon[0]) + abs(sensor[1] - beacon[1])
            disks.append((sensor, m_dist))
    return disks


def solver(disks, maxsize):
    print("solve")
    tested = {}
    scan_disks = sorted(disks, key=lambda d: d[1], reverse=True)  # big disk first
    for disk in scan_disks[::-1]:
        print("disk", disk)
        for pos in circle(disk, maxsize):
            if pos not in tested:
                tested[pos] = False
                continue
            # only points on intersections of 4 circles are candidates
            # but checking after 2 intersections is faster
            if tested[pos]:
                continue

            for scan in scan_disks:
                if detected(pos, scan):
                    break
            else:
                return pos
            tested[pos] = True


def part_two(filename, maxsize):
    disks = parser(filename)
    result = solver(disks, maxsize)
    print("frequency:", result[0] * 4_000_000 + result[1])


part_one(data_filename, 2_000_000)
part_two(data_filename, 4_000_000)
