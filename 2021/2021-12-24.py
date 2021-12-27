#!/usr/bin/python3


# this is the computation, unused in solution
def solve(w, z, a, b, c):
    z1 = z // c
    return z1 if w == z % 26 + a else 26 * z1 + w + b


#
# zs = invert(w, z2, a, b, c)
# all(solve(w, z, a, b, c) == z2 for z in zs) == True
#
def invert(w, z2, a, b, c):
    zs = []
    z0 = z2 - w - b
    if z0 % 26 == 0:
        zs.append(z0 // 26 * c)
    if 0 <= w - a < 26:
        zs.append(z2 * c + w - a)
    return zs


def solve(solutions):
    # parsed from input
    aparams = [10, 13, 13, -11, 11, -4, 12, 12, 15, -2, -5, -11, -13, -10]
    bparams = [13, 10, 3, 1, 9, 3, 5, 1, 0, 13, 7, 15, 12, 8]
    cparams = [1, 1, 1, 26, 1, 26, 1, 1, 1, 26, 26, 26, 26, 26]

    for a, b, c in zip(aparams[::-1], bparams[::-1], cparams[::-1]):
        solutions.append({})
        for w2, z2 in solutions[-2]:
            for w in range(1, 10):
                zs = invert(w, z2, a, b, c)
                for z in zs:
                    solutions[-1][(w, z)] = solutions[-1].get((w, z), []) + [(w2, z2)]


def output(solutions, fn):
    w_z = fn(solutions[-1].keys())
    for solution in solutions[:0:-1]:
        print(w_z[0], end="")
        w_z = fn(solution[w_z])
    print()


#
# (w2, z2) = [(w,z), (w,z), ...]
# where solve(w,z) = z2
#
solutions = [{(0, 0): []}]
solve(solutions)

# output max
print("max: ", end="")
output(solutions, max)

# output min
print("min: ", end="")
output(solutions, min)
