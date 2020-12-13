#!/usr/bin/python3
from collections import Counter
filename = 'q10.dat'

with open(filename, 'r') as f:
    nums = [int(l.rstrip()) for l in f.readlines()]

nums.sort()
nums = [0] + nums + [nums[-1] + 3]
print('transformers:', nums)

diffs = []
for i in range(1, len(nums)):
    diffs.append(nums[i] - nums[i-1])
print('steps:', diffs)

cnt = Counter(diffs)
print(cnt)
print('product:', cnt[1] * cnt[3])
