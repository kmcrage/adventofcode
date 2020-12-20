#!/usr/bin/python3
from itertools import permutations, product

filename = 'q20.dat'
tiles = {}

monster = ['                  # ',
           '#    ##    ##    ###',
           ' #  #  #  #  #  #   ']


def check_monster(grid, gi, gj):
    cnt = 0
    all_monster_cells = set()
    for o in range(8):
        try:
            monster_cells = set()
            for mi in range(len(monster)):
                for mj in range(len(monster[0])):
                    if monster[mi][mj] != '#':
                        continue

                    ii = mi
                    jj = mj
                    for _ in range(o // 4):
                        jj = -jj
                    for _ in range(o % 4):
                        ii, jj = jj, -ii

                    monster_cells.add((gi + ii, gj + jj))
                    if grid[gi + ii][gj + jj] != '#':
                        # print(gi, gj, mi, mj, o)
                        raise Exception("fail")
            else:
                cnt += 1
                all_monster_cells |= monster_cells
        except Exception as e:
            pass
    return cnt, all_monster_cells


def check_tile_right(tile_one, orient_one, tile_two, orient_two):
    edge_one = (orient_one + 1) % 4
    edge_two = (orient_two + 3) % 4
    if orient_one > 3:
        edge_one += 4
    if orient_two > 3:
        edge_two += 4

    return tiles[tile_one][edge_one] == tiles[tile_two][edge_two][::-1]


def check_tile_down(tile_one, orient_one, tile_two, orient_two):
    edge_one = (orient_one + 2) % 4
    edge_two = (orient_two + 0) % 4
    if orient_one > 3:
        edge_one += 4
    if orient_two > 3:
        edge_two += 4

    return tiles[tile_one][edge_one] == tiles[tile_two][edge_two][::-1]


def add_tile(layout, remaining):
    # print('add', layout, remaining)
    if not remaining:
        return layout

    side = int(len(tiles) ** 0.5)
    i = len(layout) % side
    j = len(layout) // side

    candidates = []
    if i > 0 and layout[-1] in right_matches:
        candidates = right_matches[layout[-1]]
    if j > 0 and layout[-side] in down_matches:
        if candidates:
            candidates = [m for m in down_matches[layout[-side]] if m in candidates]
        else:
            candidates = down_matches[layout[-side]]

    # print('cand', len(layout), len(candidates), layout, i, j, candidates, len(remaining), remaining)
    for c in candidates:
        if c[0] not in remaining:
            continue
        remaining_next = remaining
        remaining_next.remove(c[0])
        new_layout = layout + [c]
        r = add_tile(new_layout, remaining_next)
        if r:
            return r

    # print(layout)
    return False


def search():
    for tile in tiles:
        for o in range(8):
            rem = set(tiles.keys())
            rem.remove(tile)
            result = add_tile([(tile, o)], rem)
            if result:
                return result


#
# parse the tiles
#
with open(filename, 'r') as f:
    tile = None
    for line in [l.rstrip() for l in f.readlines()]:
        if not line:
            continue

        if line.startswith('Tile'):
            tile = int(line[5:-1])
            continue

        # first row, NESW, flip NESW
        if tile not in tiles:
            tiles[tile] = [line, line[-1], line[::-1], line[0],
                           line[::-1], line[0], line, line[-1],
                           []]
            continue

        tiles[tile][1] += line[-1]
        tiles[tile][2] = line[::-1]
        tiles[tile][3] = line[0] + tiles[tile][3]

        tiles[tile][5] += line[0]
        tiles[tile][6] = line
        tiles[tile][7] = line[-1] + tiles[tile][7]

        if len(tiles[tile][1]) > 1 and len(tiles[tile][1]) != len(tiles[tile][0]):
            tiles[tile][8] += [line[1:-1]]

print('num tiles:', len(tiles))
print(tiles)

#
# find pairs of tiles that edge-match
#
right_matches = {}
down_matches = {}
for tile in tiles:
    for sile in tiles:
        if sile == tile:
            continue

        for i in range(8):
            for j in range(8):
                if check_tile_right(tile, i, sile, j):
                    if (tile, i) not in right_matches:
                        right_matches[(tile, i)] = []
                    right_matches[(tile, i)] += [(sile, j)]

                if check_tile_down(tile, i, sile, j):
                    if (tile, i) not in down_matches:
                        down_matches[(tile, i)] = []
                    down_matches[(tile, i)] += [(sile, j)]

#
# search for a solution
#
result = search()
print(result)
corner = int(len(result) ** 0.5)
print('product', result[0][0] * result[corner - 1][0] * result[-corner][0] * result[-1][0])

#
# create an image
#
grid = []
for _ in range(8 * corner):
    grid.append([' '] * (8 * corner))

for idx, tile_o in enumerate(result):
    i = (idx // corner) * 8
    j = (idx % corner) * 8

    tile, o = tile_o
    for ii, row in enumerate(tiles[tile][8]):
        for jj, cell in enumerate(row):
            iii, jjj = ii, jj
            for _ in range(o // 4):
                jjj = 7 - jjj
            for _ in range(o % 4):
                iii, jjj = 7 - jjj, iii
            grid[i + iii][j + jjj] = cell

#
# print the image
#
for ln in grid:
    print(' '.join(ln))

#
# hunt for monsters
#
cnt = 0
monster_cells = set()
hashes = 0
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if grid[i][j] == '#':
            hashes += 1
        c, m_cells = check_monster(grid, i, j)
        cnt += c
        monster_cells |= m_cells
print('monsters:', cnt)
print('sea:', hashes - len(monster_cells))
