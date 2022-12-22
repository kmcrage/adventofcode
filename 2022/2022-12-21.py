#!/usr/bin/python3
import collections

data_filename = "2022-12-21.dat"
# data_filename = "test.dat"


def parse(filename):
    monkeys = {}
    with open(filename, "r") as f:
        for line in f:
            tokens = line.split()
            monkeys[tokens[0][:-1]] = int(tokens[1]) if len(tokens) == 2 else tokens[1:]
    return monkeys


def compute(monkeys, target):
    unsolved = {m for m, e in monkeys.items() if not isinstance(e, (int, complex))}
    calls = {m: c for m, c in monkeys.items() if isinstance(c, (int, complex))}
    root = None
    while target in unsolved:
        for monkey in list(unsolved):
            expr = monkeys[monkey]
            if expr[0] in unsolved or expr[2] in unsolved:
                continue
            if expr[1] == "+":
                call = calls[expr[0]] + calls[expr[2]]
            elif expr[1] == "*":
                call = calls[expr[0]] * calls[expr[2]]
            elif expr[1] == "-":
                call = calls[expr[0]] - calls[expr[2]]
            elif expr[1] == "/":
                call = calls[expr[0]] / calls[expr[2]]
            calls[monkey] = call
            unsolved.remove(monkey)
            if monkey == target:
                root = call
                break
    # print(calls)
    return root, calls


def solve(monkeys, callers, source, soln):
    expr = monkeys[source]
    if isinstance(expr, complex):
        print(f"part two, computed solution: {source} = {int(soln)}")
        return

    lhs = callers[expr[0]]
    if isinstance(lhs, complex):
        rhs = callers[expr[2]]
        if expr[1] == "+":
            solve(monkeys, callers, expr[0], soln - rhs)
        elif expr[1] == "-":
            solve(monkeys, callers, expr[0], soln + rhs)
        elif expr[1] == "*":
            solve(monkeys, callers, expr[0], soln / rhs)
        elif expr[1] == "/":
            solve(monkeys, callers, expr[0], soln * rhs)

    elif expr[1] == "+":
        solve(monkeys, callers, expr[2], soln - lhs)
    elif expr[1] == "-":
        solve(monkeys, callers, expr[2], lhs - soln)
    elif expr[1] == "*":
        solve(monkeys, callers, expr[2], soln / lhs)
    elif expr[1] == "/":
        solve(monkeys, callers, expr[2], lhs / soln)


def part_two(monkeys_orig, equal, target):
    # use complax numbers to follow the target through the equations
    # then solve with linear algebra, assuming that we never divide by 
    # a complex number
    monkeys = monkeys_orig.copy()
    monkeys[target] = 1j
    _, callers = compute(monkeys, target=equal)
    lhs = callers[monkeys[equal][0]]
    rhs = callers[monkeys[equal][2]]
    soln = int((rhs.real - lhs.real) / (lhs.imag - rhs.imag))
    print(
        f"part two, complex estimate:  {target} = {soln} (assumed no division by humn)"
    )

    # solve up the tree, inverting as we go
    monkeys[equal][1] = "-"
    solve(monkeys, callers, equal, 0)


monkeys = parse(data_filename)
result = int(compute(monkeys, "root")[0])
print(f"part one, root = {result}")
part_two(monkeys, "root", "humn")
