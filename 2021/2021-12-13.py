#!/usr/bin/python3


filename = "2021-12-13.dat"


def part_all():
    dots = set()
    with open(filename, "r") as f:
        for i, line in enumerate(f.readlines()):
            line = line.strip()

            if "," in line:
                coords = tuple(int(n) for n in line.split(","))
                dots.add(coords)
                continue

            elif line == "":
                print("initial num dots:", len(dots))
                continue

            dir, fold = line.split()[-1].split("=")
            fold = int(fold)

            # perform the fold
            new_dots = set()
            for pos in dots:
                if dir == "y" and pos[1] > fold:
                    new_dots.add((pos[0], 2 * fold - pos[1]))
                elif dir == "x" and pos[0] > fold:
                    new_dots.add((2 * fold - pos[0], pos[1]))
                else:
                    new_dots.add(pos)
            dots = new_dots

            print("after ", dir, "=", fold, "num dots:", len(dots))

        print()
        size = [max(c) for c in zip(*dots)]
        for j in range(size[1] + 1):
            for i in range(size[0] + 1):
                print("#" if (i, j) in dots else " ", end="")
            print()


part_all()
