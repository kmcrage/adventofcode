#!/usr/bin/python3
from functools import lru_cache
filename = 'q10.dat'

@lru_cache(None)
def numpaths(diffs):
    if len(diffs) == 1:
        return 1

    np = numpaths(diffs[1:])
    if sum(diffs[:2]) <= 3:
        np += numpaths(diffs[2:])
    if sum(diffs[:3]) <= 3:
        np += numpaths(diffs[3:])

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
