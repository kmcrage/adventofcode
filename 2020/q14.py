#!/usr/bin/python3
import re

filename = 'q14.dat'


def apply_mask(v, mask):
    v_str = "{0:#038b}".format(v)[2:]
    for i, b in enumerate(mask):
        if b != 'X':
            v_str = v_str[:i] + b + v_str[i+1:]
    return v_str


def apply_mask_v2(memory, idx, v, mask):
    idx_str = "{0:#038b}".format(idx)[2:]
    num_x = 0
    for i, b in enumerate(mask):
        if b != '0':
            idx_str = idx_str[:i] + b + idx_str[i+1:]
        if b == 'X':
            num_x += 1

    fmt = "{0:#0" + "%s" % (num_x + 2) + "b}"
    for i in range(2**num_x):
        i_str = fmt.format(i)[2:]
        idx_bin = ''.join([a + b for a, b in zip(idx_str.split('X'), list(i_str) + [''])])
        idx_dec = int(idx_bin, 2)
        memory[idx_dec] = v


def part_one():
    mem = {}
    mask = None
    with open(filename, 'r') as f:
        for line in f.readlines():
            if line.startswith('mask ='):
                mask = line.rstrip().split(' ')[-1]

            elif line.startswith('mem'):
                m = re.search(r'\[(\d+)\]\s+=\s+(\d+)', line)
                loc = m.group(1)
                val = int(m.group(2))
                mem[loc] = apply_mask(val, mask)

    acc = 0
    for v in mem.values():
        acc += int(v, 2)
    print('pt1 sum:', acc)


def part_two():
    mem = {}
    mask = None
    with open(filename, 'r') as f:
        c = 0
        for line in f.readlines():
            c += 1
            if line.startswith('mask ='):
                mask = line.rstrip().split(' ')[-1]

            elif line.startswith('mem'):
                m = re.search(r'\[(\d+)\]\s+=\s+(\d+)', line)
                loc = int(m.group(1))
                val = int(m.group(2))
                apply_mask_v2(mem, loc, val, mask)

    acc = 0
    for v in mem.values():
        acc += v
    print('pt2 sum:', acc)


part_one()
part_two()
