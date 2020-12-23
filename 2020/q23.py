#!/usr/bin/python3
start = '315679824'


def print_cups(cup_dict, first, limit=10):
    print('cups: ', first, end='')
    n = cup_dict[first]
    cnt = 0
    while n != first and cnt < limit:
        print(n, end='')
        n = cup_dict[n]
        cnt += 1
    print()


cups = {}  # dictionary with the next number as the value
current = None
last = None
for c in [int(d) for d in start] + list(range(len(start) + 1, 1000 * 1000 + 1)):
    if not current:
        current = c
    if last:
        cups[last] = c
    last = c
cups[last] = current


most = max(cups.keys())
for move in range(10 * 1000 * 1000):
    # print('move:', move + 1)
    # print_cups(cups, current)

    # build pickup
    pickup = [cups[current],
              cups[cups[current]],
              cups[cups[cups[current]]]]
    # remove pickup
    cups[current] = cups[cups[cups[cups[current]]]]

    # find destination
    destination = current - 1
    if destination == 0:
        destination = most
    while destination in pickup:
        destination = destination - 1
        if destination == 0:
            destination = most
    # print('destination:', destination)

    # insert pickup which is already chained together
    cups[destination], cups[pickup[-1]] = pickup[0], cups[destination]

    # next
    current = cups[current]

print_cups(cups, 1)
print('result part two:', cups[1] * cups[cups[1]])
