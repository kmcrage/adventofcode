#!/usr/bin/python3

data_filename = "2022-12-08.dat"
#data_filename = "test.dat"


def parser(filename):
    trees = {}
    with open(filename, "r") as f:
        for row, line in enumerate(f):
            for col, h in enumerate(line.strip()):
                trees[(col, row)] = h
    return trees


def part_one(trees):
    visible = set()
    for dirn in ((1, 0), (-1, 0), (0, 1), (0, -1)):
        for tree in trees:
            if tree in visible:
                continue
            
            pos = (tree[0] + dirn[0], tree[1] + dirn[1])
            while pos in trees and trees[pos] < trees[tree]:
                pos = (pos[0] + dirn[0], pos[1] + dirn[1])

            if pos not in trees:
                visible.add(tree)

    print("Visible:", len(visible))


def part_two(trees):
    max_area = 0
    for tree in trees:
        area = 1
        for dirn in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            pos = (tree[0] + dirn[0], tree[1] + dirn[1])
            if pos not in trees:
                continue

            num = 0 # count the number of non-blocking trees
            while pos in trees and trees[pos] < trees[tree]:
                pos = (pos[0] + dirn[0], pos[1] + dirn[1])
                num += 1

            if pos in trees:
                num += 1 # add in the blocking tree

            area *= num
        if area > max_area:
            max_area = area

    print(f"Max Area: {max_area}")


trees = parser(data_filename)
part_one(trees)
part_two(trees)
