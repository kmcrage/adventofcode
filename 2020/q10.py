#!/usr/bin/python3
from functools import lru_cache
filename = 'q10.dat'


@lru_cache(None)
def numpaths(steps):
    if len(steps) == 1:
        return 1

    np = 0
    for lookahead in range(1, 4):
        if sum(steps[:lookahead]) <= 3:
            np += numpaths(steps[lookahead:])

    return np


with open(filename, 'r') as f:
    nums = [int(l.rstrip()) for l in f.readlines()]

nums.sort()
nums = [0] + nums + [nums[-1] + 3]
print('transformers:', nums)

diffs = []
for i in range(1, len(nums)):
    diffs.append(nums[i] - nums[i-1])
diffs = tuple(diffs)  # so its hashable
print('difs:', diffs)

print('paths:', numpaths(diffs))
