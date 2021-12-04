#!/usr/bin/python3


filename = "2021-12-04.dat"


def read_boards(f):
    boards = []
    for line in f.readlines():
        line = line.strip()
        if line == "":
            boards.append([])
            continue

        boards[-1].append(line.split())
    return boards


def score(board, seen, number):
    cnt = 0
    for row in board:
        for num in row:
            if num not in seen:
                cnt += int(num)
    print("cnt", cnt, "number", number, "product", cnt * int(number))
    exit()


def part_one():
    with open(filename, "r") as f:
        numbers = f.readline().strip().split(",")
        boards = read_boards(f)

        seen = set()
        for number in numbers:
            seen.add(number)
            print("draw", number)
            for board in boards:
                for row in board:
                    if not [n for n in row if n not in seen]:
                        print("halt on row")
                        score(board, seen, number)

                for col in range(len(board[0])):
                    if not [row[col] for row in board if row[col] not in seen]:
                        print("halt on col")
                        score(board, seen, number)


def part_two():
    with open(filename, "r") as f:
        numbers = f.readline().strip().split(",")
        boards = read_boards(f)

        seen = set()
        solved = set()
        for number in numbers:
            seen.add(number)
            print("draw", number)
            for b, board in enumerate(boards):
                if b in solved:
                    continue
                for row in board:
                    if not [n for n in row if n not in seen]:
                        print("halt on row")
                        solved.add(b)
                        if len(solved) == len(boards):
                            score(board, seen, number)

                for col in range(len(board[0])):
                    if not [row[col] for row in board if row[col] not in seen]:
                        print("halt on col")
                        solved.add(b)
                        if len(solved) == len(boards):
                            score(board, seen, number)


# part_one()
part_two()
