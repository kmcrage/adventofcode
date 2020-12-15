#!/usr/bin/python3
filename = 'q15.dat'

nums = {}
curr = None
last = None
turn = 0
with open(filename, 'r') as f:
    for n in f.readline().rstrip().split(','):
        last = curr
        curr = int(n)
        turn += 1
        nums[curr] = {'count': 1, 'idx': turn}
        # print('turn', turn, 'num', curr)

while turn < 30000000:
    last = curr
    if nums[last]['count'] == 1:
        curr = 0
    else:
        curr = turn - nums[last]['idx']
        nums[last]['idx'] = turn

    turn += 1
    if curr not in nums:
        nums[curr] = {'count': 1, 'idx': turn}
    nums[curr]['count'] += 1
    if turn == 2020:
        print('turn', turn, 'num', curr)
print('turn', turn, 'num', curr)
