#!/usr/bin/python
from collections import Counter
filename = 'q10.dat'

with open(filename, 'r') as f:
    nums = [int(l.rstrip()) for l in f.readlines()]

nums.sort()
nums = [0] + nums + [nums[-1] + 3]
print nums

diffs = []
for i in range(1, len(nums)):
    diffs.append(nums[i] - nums[i-1])
print diffs

cnt = Counter(diffs)
print cnt
print cnt[1] * cnt[3]
