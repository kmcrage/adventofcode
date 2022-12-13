#!/usr/bin/python3
import collections
import copy
import functools
import math

data_filename = "2022-12-11.dat"
# data_filename = "test.dat"


def parse(filename):
    monkeys = []

    with open(filename, "r") as f:
        monkey = {}
        for line in f:
            tokens = line.strip().split()
            if len(tokens) == 0:
                monkeys.append(monkey)
                monkey = {}
            elif tokens[0] == "Monkey":
                monkey = {"inspected": 0}
            elif tokens[0] == "Starting":
                monkey["items"] = collections.deque(int(i.strip(",")) for i in tokens[2:])
            elif tokens[0] == "Operation:":
                monkey["operation"] = " ".join(tokens[3:])
            elif tokens[0] == "Test:":
                monkey["test"] = int(tokens[-1])
            elif tokens[1] == "true:":
                monkey["true"] = int(tokens[-1])
            elif tokens[1] == "false:":
                monkey["false"] = int(tokens[-1])
        if monkey:
            monkeys.append(monkey)

    return monkeys


@functools.lru_cache(maxsize=None)
def update_item(old, op, divisor, modulo):
    new = eval(op) // divisor
    new = new % modulo
    return new


def part_one(monkeys, rounds, divisor):
    # work mod lcf of test modulos doesn't change any tests
    # lcm(a,b) * gcd(a,b) = a * b
    lcm = 1
    for monkey in monkeys:
        lcm = (lcm * monkey["test"]) // math.gcd(lcm, monkey["test"])
    # print('lcm:', lcm)

    for _ in range(rounds):
        for monkey in monkeys:
            while monkey["items"]:
                monkey["inspected"] += 1
                item = monkey["items"].popleft()
                item = update_item(item, monkey["operation"], divisor, lcm)
                if item % monkey["test"] == 0:
                    monkeys[monkey["true"]]["items"].append(item)
                else:
                    monkeys[monkey["false"]]["items"].append(item)

    # for monkey in monkeys:
    #     print(monkey["inspected"])
    inspected = [m["inspected"] for m in monkeys]
    inspected.sort(reverse=True)
    print("inspections:", sum(inspected))
    print("monkey business:", inspected[0] * inspected[1])


monkeys = parse(data_filename)
part_one(copy.deepcopy(monkeys), 20, 3)
part_one(copy.deepcopy(monkeys), 10_000, 1)
