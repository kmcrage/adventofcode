#!/usr/bin/python3
import re
from math import gcd

filename = 'q13.dat'


def lcm(a, b):
    return a * b // gcd(a, b)


def part_one():
    with open(filename, 'r') as f:
        start = int(f.readline().rstrip())
        buses = [int(b) for b in re.findall(r'\d+', f.readline())]

    min_wait = 99999
    min_bus = None
    for bus in buses:
        wait = -start % bus
        if wait < min_wait:
            min_wait = wait
            min_bus = bus

    print("wait:", min_wait, "for bus", min_bus)
    print('product:', min_wait * min_bus)


def part_two():
    with open(filename, 'r') as f:
        f.readline()

        buses = {}
        for i, b in enumerate(f.readline().rstrip().split(',')):
            if b != 'x':
                buses[int(b)] = i

    t = 0
    step = 1
    active = list(buses.keys())
    while active:
        t += step
        rm = []
        for i, b in enumerate(active):
            if (t + buses[b]) % b:
                continue
            step = lcm(step, b)
            print('bus', b, 'arrives at time', t, 'new timestep', step)
            rm.append(i)
        for i in rm:
            active.pop(i)

    print('time', t)


part_one()
part_two()
