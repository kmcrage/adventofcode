#!/usr/bin/python3
import collections

data_filename = "2022-12-20.dat"
# data_filename = "test.dat"


def parse(filename):
    nums = []
    with open(filename, "r") as f:
        nums.extend(int(line) for line in f if line.strip())
    return nums


def part_one(nums, key=1, mixes=1):
    # the list could have repeated numbers in it
    ref = [(i, n * key) for i, n in enumerate(nums)]
    queue = collections.deque(ref)
    sz = len(queue)
    zero = None

    for _ in range(mixes):
        #print(queue)
        for e_num in ref:
            # print(queue)
            idx = queue.index(e_num)
            if e_num[1] == 0:
                zero = e_num
            queue.rotate(-idx)
            queue.popleft()
            queue.insert(e_num[1] % (sz - 1), e_num)

    idx_zero = queue.index(zero)
    queue.rotate(-idx_zero)
    #print(queue)
    print([queue[idx % len(queue)] for idx in (1_000, 2_000, 3_000)])
    result = sum(queue[idx % len(queue)][1] for idx in (1_000, 2_000, 3_000))
    print(f"mixes: {mixes}, key: {key}, result: {result}")


nums = parse(data_filename)
part_one(nums)
part_one(nums, mixes=10, key=811589153)