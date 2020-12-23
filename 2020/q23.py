#!/usr/bin/python3
from collections import deque
start = '389125467'

cups = deque([int(c) for c in start])
for c in range(10, 1000001):
    cups.append(c)

move = 0
mx = len(cups)
while move < 1000:
    move += 1
    if move % 10000 == 0:
        print('move:', move)
    #print('cups:', cups)
    current = cups[0]
    cups.rotate(-1)
    pickup = deque()
    for _ in range(3):
        pickup.appendleft(cups.popleft())
    # print('pickup:', pickup)
    destination = current - 1
    if destination == 0:
        destination = mx
    while destination in pickup:
        destination = destination - 1
        if destination == 0:
            destination = mx
    # print('destination:', destination)
    idx = cups.index(destination)
    cups.rotate(-1 - idx)
    cups.extendleft(pickup)
    cups.rotate(idx + 1)

# print('final:', cups)
cups.rotate(-cups.index(1))
cups.popleft()
# print('result part one:', ''.join([str(c) for c in cups]))
print('result part two:', cups[0] * cups[1])
