#!/usr/bin/python3
filename = 'q9.dat'

with open(filename, 'r') as f:
    nums = [int(l.rstrip()) for l in f.readlines()]

preamble = 25
for idx in range(preamble, len(nums)):
    tgt = nums[idx]
    for i, n in enumerate(nums[idx - preamble:idx - 1], idx - preamble):
        m = tgt - n
        # print idx, i, n, nums[i + 1:idx], tgt
        if m in nums[i + 1:idx]:
            # print n, m, tgt
            break
    else:
        print('invalid number', tgt)
        candidates = nums
        tgt_sum = [candidates.pop(0)]
        while candidates and sum(tgt_sum) != tgt:
            if sum(tgt_sum) > tgt:
                tgt_sum.pop(0)
            else:
                tgt_sum.append(candidates.pop(0))
        if sum(tgt_sum) == tgt:
            print('range of values', tgt_sum)
            print('min + max =', min(tgt_sum) + max(tgt_sum))
            exit(0)
