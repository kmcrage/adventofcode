#!/usr/bin/python3

from functools import reduce
from operator import mul

filename = "2021-12-16.dat"


def literal(binary):
    pver = int(binary[:3], 2)
    print("literal version", pver)
    binary = binary[6:]

    num = ""
    while binary[0] == "1":
        num += binary[1:5]
        binary = binary[5:]
    num += binary[1:5]
    binary = binary[5:]

    value = int(num, 2)
    print("literal:", value)
    return value, pver, binary


def operate(op, values):
    if op == 0:
        value = sum(values)
    elif op == 1:
        value = reduce(mul, values, 1)
    elif op == 2:
        value = min(values)
    elif op == 3:
        value = max(values)
    elif op == 5:
        value = 1 if values[0] > values[1] else 0
    elif op == 6:
        value = 1 if values[0] < values[1] else 0
    elif op == 7:
        value = 1 if values[0] == values[1] else 0
    return value


def operator(binary):
    pver = int(binary[:3], 2)
    print("operator version", pver)
    ptype = int(binary[3:6], 2)
    vsum = pver
    binary = binary[6:]

    values = []
    if binary[0] == "0":
        length = int(binary[1:16], 2)
        print("subpacket length:", length)
        binary = binary[16:]
        subbinary = binary[:length]
        while len(subbinary) > 3:
            subval, subvsum, subbinary = parse(subbinary)
            vsum += subvsum
            values.append(subval)
        binary = binary[length:]

    else:
        subpackets = int(binary[1:12], 2)
        binary = binary[12:]
        print("subpackets:", subpackets)
        for _ in range(subpackets):
            subval, subvsum, binary = parse(binary)
            vsum += subvsum
            values.append(subval)

    return operate(ptype, values), vsum, binary


def parse(binary):
    ptype = int(binary[3:6], 2)
    if ptype == 4:
        return literal(binary)
    else:
        return operator(binary)


with open(filename, "r") as f:
    for line in f.readlines():
        line = line.strip()
        print(line)

        binary = "".join(format(int(d, 16), "04b") for d in line)
        value, vsum, _ = parse(binary)

        print()
        print("Part 1, version sum:", vsum)
        print("Part 2, value:", value)
        print()
