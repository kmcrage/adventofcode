#!/usr/bin/python3
start = '315679824'


class Cup:
    def __init__(self, value):
        self.value = value
        self.next = None


def print_cups(cup):
    print('cups: ', cup.value, end='')
    n = cup.next
    while n.value != cup.value:
        print(n.value, end='')
        n = n.next
    print()


#
# build linked list on top of a hash for faster indexing
#
cups = {}
previous = None
current = None
for c in [int(d) for d in start]:
    cups[c] = Cup(c)
    if not current:
        current = cups[c]
    if previous:
        previous.next = cups[c]
    previous = cups[c]
for c in range(len(start) + 1, 1000001):
    cups[c] = Cup(c)
    previous.next = cups[c]
    previous = cups[c]
previous.next = current


most = max(cups.keys())
for move in range(10000000):
    if move % 100000 == 0:
        print('move:', move + 1)
    # print_cups(current)

    # build pickup
    pickup = [current.next, current.next.next, current.next.next.next]
    pickup_values = {p.value for p in pickup}
    # remove pickup
    current.next = current.next.next.next.next

    # find destination
    destination = current.value - 1
    if destination == 0:
        destination = most
    while destination in pickup_values:
        destination = destination - 1
        if destination == 0:
            destination = most
    # print('destination:', destination)

    # insert pickup which is already chained together
    end = cups[destination].next
    cups[destination].next = pickup[0]
    pickup[-1].next = end

    # next
    current = current.next

# print_cups(cups[1])
print('result part two:', cups[1].next.value * cups[1].next.next.value)
