#!/usr/bin/python3
filename = 'q17.dat'

space = {}
size = [1, 1, 0, 0]


def active(space, x, y, z, w):
    cnt = 0
    for i in range(x-1, x+2):
        if i not in space:
            continue
        for j in range(y-1, y+2):
            if j not in space[i]:
                continue
            for k in range(z-1, z+2):
                if k not in space[i][j]:
                    continue
                for m in range(w-1, w+2):
                    if m not in space[i][j][k]:
                        continue
                    if i == x and j == y and k == z and m == w:
                        continue
                    if space[i][j][k][m] == '#':
                        cnt += 1
    return cnt


with open(filename, 'r') as f:
    space[0] = {0: {}}
    for i, state in enumerate(f.readlines()):
        space[0][0][i] = {j: s for j, s in enumerate(state)}
        size[2] = i + 1
        size[3] = len(state)

print('initialised')
if False:
    for x in sorted(space.keys()):
        print('x:', x)
        for y in sorted(space[x].keys()):
            for z in sorted(space[x][y].keys()):
                print(space[x][y][z], end='')
            print()

for cycle in range(1, 7):
    activate = []
    deactivate = []
    for x in range(-cycle, size[0] + cycle):
        for y in range(-cycle, size[1] + cycle):
            for z in range(-cycle, size[2] + cycle):
                for w in range(-cycle, size[3] + cycle):
                    act = active(space, x, y, z, w)
                    # print(x, y, z, act)
                    if x not in space or y not in space[x] or z not in space[x][y] or w not in space[x][y][z] or \
                            space[x][y][z][w] == '.':
                        if act == 3:
                            activate.append((x, y, z, w))
                            # print('activated')
                    else:
                        if act < 2 or act > 3:
                            deactivate.append((x, y, z, w))
                            # print('de-activated')

    for coord in activate:
        if coord[0] not in space:
            space[coord[0]] = {}
        if coord[1] not in space[coord[0]]:
            space[coord[0]][coord[1]] = {}
        if coord[2] not in space[coord[0]][coord[1]]:
            space[coord[0]][coord[1]][coord[2]] = {}
        space[coord[0]][coord[1]][coord[2]][coord[3]] = '#'
    for coord in deactivate:
        if coord[0] in space and coord[1] in space[coord[0]] and \
                coord[2] in space[coord[0]][coord[1]] and coord[3] in space[coord[0]][coord[1]][coord[2]]:
            space[coord[0]][coord[1]][coord[2]][coord[3]] = '.'

    print('cycle:', cycle)
    cnt = 0
    for x in sorted(space.keys()):
        # print('x:', x)
        for y in sorted(space[x].keys()):
            for z in sorted(space[x][y].keys()):
                for w in sorted(space[x][y][z].keys()):
                    # print(space[x][y][z], end='')
                    if space[x][y][z][w] == '#':
                        cnt += 1
            # print()
    print('active:', cnt)
