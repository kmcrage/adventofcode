#!/usr/bin/python3

filename = 'q24.dat'
tiles = {(0, 0): False}

moves = {'e': (0, 2),
         'se': (-1, 1),
         'sw': (-1, -1),
         'w': (0, -2),
         'nw': (1, -1),
         'ne': (1, 1)}

with open(filename, 'r') as f:
    for line in f.readlines():
        line = line.strip()
        pos = [0, 0]
        while line:
            for move, dr in moves.items():
                if not line.startswith(move):
                    continue
                pos[0] += dr[0]
                pos[1] += dr[1]
                line = line[len(move):]
        p = tuple(pos)
        if p in tiles:
            tiles[p] = not tiles[p]
        else:
            tiles[p] = True

print('black tiles:', len([t for t in tiles.values() if t]))

for day in range(1, 101):
    # which tiles need checking?
    checklist = set()
    for p in tiles.keys():
        checklist.add(p)
        for m in moves.values():
            q = (p[0] + m[0], p[1] + m[1])
            checklist.add(q)

    flips = {}
    # check the tiles
    for p in checklist:
        num_black = 0
        for m in moves.values():
            q = (p[0] + m[0], p[1] + m[1])
            if q in tiles and tiles[q]:
                num_black += 1
        if p in tiles and tiles[p] and (num_black == 0 or num_black > 2):
            flips[p] = False
        if (p not in tiles or not tiles[p]) and num_black == 2:
            flips[p] = True

    # flip the tiles
    for p, t in flips.items():
        if p in tiles:
            tiles[p] = t
        else:
            if t:
                tiles[p] = t

    # stats
    print('day', day, 'black tiles:', len([t for t in tiles.values() if t]))