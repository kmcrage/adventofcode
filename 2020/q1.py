#!/usr/bin/python
filename = 'q1.dat'

nums_list = []
with open(filename, 'r') as f:
    nums_list = [l.rstrip() for l in f.readlines()]

print nums_list
nums_set = set(nums_list)
while nums_list:
    n = nums_list.pop()
    for m in nums_list:
        p = 2020 - int(n) - int(m)
        if str(p) in nums_set:
            print n, m, p, int(n) * int(m) * p
            exit()

