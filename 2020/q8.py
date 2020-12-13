#!/usr/bin/python3
filename = 'q8.dat'

with open(filename, 'r') as f:
    prog = [l.rstrip() for l in f.readlines()]

for err in range(len(prog)):
    if prog[err].startswith('acc'):
        continue

    accumulator = 0
    ln = 0
    visited = set()
    while ln < len(prog):
        if ln in visited:
            break
        visited.add(ln)

        op, arg = prog[ln].split(' ')
        if op == 'acc':
            accumulator += int(arg)
            ln += 1
        elif (ln == err and op == 'acc') or (ln != err and op == 'jmp'):
            ln += int(arg)
        else:
            ln += 1
    else:
        print('changed line:', err)
        print('accumulator:', accumulator)
        exit(0)
