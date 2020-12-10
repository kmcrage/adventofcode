#!/usr/bin/python3
from functools import lru_cache
filename = 'q10.dat'

@lru_cache(None)
def numpaths(diffs):
    if len(diffs) == 1:
        return 1

    np = 0
    for lookahead in range(1, 4):
        if sum(diffs[:lookahead]) <= 3:
            np += numpaths(diffs[lookahead:])

    return np


with open(filename, 'r') as f:
    nums = [int(l.rstrip()) for l in f.readlines()]

nums.sort()
nums = [0] + nums + [nums[-1] + 3]
print(nums)

diffs = []
for i in range(1, len(nums)):
    diffs.append(nums[i] - nums[i-1])
diffs = tuple(diffs)  # so its hashable
print(diffs)

print(numpaths(diffs))
