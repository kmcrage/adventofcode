#!/usr/bin/python3

data_filename = "2022-12-22.dat"
# data_filename = "test2.dat"

directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
arrows = ">v<^"


def parse(filename):
    plan = {}
    path = None
    row = 0
    with open(filename, "r") as f:
        for line in f:
            line = line.rstrip()

            if "R" in line or "L" in line:
                path = line
                continue

            if "." in line or "#" in line:
                for col, cell in enumerate(line):
                    if cell == " ":
                        continue
                    plan[(row, col)] = cell
                row += 1

    pathlist = []
    while path:
        if path[0] in ("R", "L"):
            pathlist.append(path[0])
            path = path[1:]
        else:
            dist = path.split("R")[0].split("L")[0]
            path = path[len(dist) :]
            pathlist.append(int(dist))

    return plan, pathlist


def visualise(plan):
    face_size = int(pow(len(plan) // 6, 0.5))
    for i in range(4 * face_size + 1):
        for j in range(4 * face_size + 1):
            if (i, j) in plan:
                print(plan[(i, j)], end="")
            else:
                print(" ", end="")
        print("")


def part_one(plan_orig, path_orig, directions):
    plan = plan_orig.copy()
    pos = (0, min(j for (i, j) in plan if i == 0))
    path = path_orig.copy()
    facing = 0

    while path:
        dist = path.pop(0)
        if dist == "R":
            facing = (facing + 1) % 4
            continue
        if dist == "L":
            facing = (facing - 1) % 4
            continue

        for _ in range(dist):
            new_pos = (pos[0] + directions[facing][0], pos[1] + directions[facing][1])
            if new_pos not in plan:
                # facing = >v<^
                if facing == 0:
                    new_pos = (pos[0], min(j for (i, j) in plan if i == pos[0]))
                elif facing == 1:
                    new_pos = (min(i for (i, j) in plan if j == pos[1]), pos[1])
                elif facing == 2:
                    new_pos = (pos[0], max(j for (i, j) in plan if i == pos[0]))
                elif facing == 3:
                    new_pos = (max(i for (i, j) in plan if j == pos[1]), pos[1])
            if plan[new_pos] == "#":
                break
            pos = new_pos
            plan[pos] = arrows[facing]

    result = 1_000 * (1 + pos[0]) + 4 * (1 + pos[1]) + facing
    visualise(plan)
    print(f"part_one: {result}")


def teleport(pos, facing, plan):
    # separate out the faces and the coords in the face
    # doing the maths this way is less error prone
    # and easier to check
    face_size = int(pow(len(plan) // 6, 0.5))
    face = (pos[0] // face_size, pos[1] // face_size)
    face_pos = (pos[0] % face_size, pos[1] % face_size)
    # facing arrows = ">v<^"
    # print(face, facing)

    if face == (0, 1):
        if facing == 2:  # <
            new_face = (2, 0)
            new_facing = 0  # >
            new_face_pos = (face_size - 1 - face_pos[0], 0)
        elif facing == 3:  # ^
            new_face = (3, 0)
            new_facing = 0  # >
            new_face_pos = (face_pos[1], 0)

    elif face == (0, 2):
        if facing == 0:  # >
            new_face = (2, 1)
            new_facing = 2  # <
            new_face_pos = (face_size - 1 - face_pos[0], face_size - 1)
        elif facing == 1:  # v
            new_face = (1, 1)
            new_facing = 2  # <
            new_face_pos = (face_pos[1], face_size - 1)
        elif facing == 3:  # ^
            new_face = (3, 0)
            new_facing = 3  # ^
            new_face_pos = (face_size - 1, face_pos[1])

    elif face == (1, 1):
        if facing == 0:  # >
            new_face = (0, 2)
            new_facing = 3  # ^
            new_face_pos = (face_size - 1, face_pos[0])
        elif facing == 2:  # <
            new_face = (2, 0)
            new_facing = 1  # v
            new_face_pos = (0, face_pos[0])

    elif face == (2, 0):
        if facing == 2:  # <
            new_face = (0, 1)
            new_facing = 0  # >
            new_face_pos = (face_size - 1 - face_pos[0], 0)
        elif facing == 3:  # ^
            new_face = (1, 1)
            new_facing = 0  # >
            new_face_pos = (face_pos[1], 0)

    elif face == (2, 1):
        if facing == 0:  # >
            new_face = (0, 2)
            new_facing = 2  # <
            new_face_pos = (face_size - 1 - face_pos[0], face_size - 1)
        elif facing == 1:  # v
            new_face = (3, 0)
            new_facing = 2  # <
            new_face_pos = (face_pos[1], face_size - 1)

    elif face == (3, 0):
        if facing == 0:  # >
            new_face = (2, 1)
            new_facing = 3  # ^
            new_face_pos = (face_size - 1, face_pos[0])
        elif facing == 1:  # v
            new_face = (0, 2)
            new_facing = 1  # v
            new_face_pos = (0, face_pos[1])
        elif facing == 2:  # <
            new_face = (0, 1)
            new_facing = 1  # v
            new_face_pos = (0, face_pos[0])

    new_pos = (
        new_face[0] * face_size + new_face_pos[0],
        new_face[1] * face_size + new_face_pos[1],
    )
    return new_pos, new_facing


def part_two(plan_orig, path_orig, directions):
    plan = plan_orig.copy()
    pos = (0, min(j for (i, j) in plan if i == 0))
    path = path_orig.copy()
    facing = 0

    while path:
        dist = path.pop(0)
        if dist == "R":
            facing = (facing + 1) % 4
            continue
        if dist == "L":
            facing = (facing - 1) % 4
            continue
        for _ in range(dist):
            new_pos = (pos[0] + directions[facing][0], pos[1] + directions[facing][1])
            new_facing = facing
            if new_pos not in plan:
                new_pos, new_facing = teleport(pos, facing, plan)
            if plan[new_pos] == "#":
                break
            pos = new_pos
            facing = new_facing
            plan[pos] = arrows[facing]
    result = 1_000 * (1 + pos[0]) + 4 * (1 + pos[1]) + facing
    visualise(plan)
    print(f"part_two: {result}")


plan, path = parse(data_filename)
part_one(plan, path, directions)
part_two(plan, path, directions)
