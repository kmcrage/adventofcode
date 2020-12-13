#!/usr/bin/python3
filename = 'q12.dat'

DIR = {'N': [1, 0], 'E': [0, 1], 'S': [-1, 0], 'W': [0, -1]}
ROT = ['N', 'E', 'S', 'W']


def part_one():
    p = [0, 0]
    dr = 'E'
    with open(filename, 'r') as f:
        for ins in f:
            ins = ins.rstrip()
            num = int(ins[1:])

            if ins[0] == 'N':
                p[0] += num
            elif ins[0] == 'E':
                p[1] += num
            elif ins[0] == 'S':
                p[0] -= num
            elif ins[0] == 'W':
                p[1] -= num
            elif ins[0] == 'F':
                p[0] += num * DIR[dr][0]
                p[1] += num * DIR[dr][1]
            elif ins[0] == 'R':
                dr = ROT[(ROT.index(dr) + num//90) % len(ROT)]
            elif ins[0] == 'L':
                dr = ROT[(ROT.index(dr) - num//90) % len(ROT)]
            print(ins, p)

    print('pos', p)
    print('dist', abs(p[0]) + abs(p[1]))


def part_two():
    w = [1, 10]
    p = [0, 0]
    with open(filename, 'r') as f:
        for ins in f:
            ins = ins.rstrip()
            num = int(ins[1:])

            if ins[0] == 'N':
                w[0] += num
            elif ins[0] == 'E':
                w[1] += num
            elif ins[0] == 'S':
                w[0] -= num
            elif ins[0] == 'W':
                w[1] -= num
            elif ins[0] == 'F':
                p[0] += num * w[0]
                p[1] += num * w[1]
            elif ins[0] == 'R':
                for _ in range(0, num//90):
                    w = [-w[1], w[0]]
            elif ins[0] == 'L':
                for _ in range(0, num//90):
                    w = [w[1], -w[0]]
            print(ins, p)

    print('pos', p)
    print('waypoint', w)
    print('dist', abs(p[0]) + abs(p[1]))


part_one()
part_two()
