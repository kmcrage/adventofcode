#!/usr/bin/python3


from collections import Counter


filename = "2021-12-03.dat"


def part_one():
    report = []
    with open(filename, "r") as f:
        for line in f.readlines():
            num = line.strip()
            for i in range(len(num)):
                if i >= len(report):
                    report.append([])
                report[i].append(num[i])

    gamma = "".join(Counter(report[i]).most_common()[0][0] for i in range(len(report)))
    print("gamma", gamma)
    epsilon = gamma.replace("0", "a").replace("1", "0").replace("a", "1")
    print("epsilon", epsilon)
    print("result", int(gamma, 2) * int(epsilon, 2))


def rating(report, default, idx):
    i = 0
    candidates = report
    while len(candidates) > 1:
        commons = Counter(n[i] for n in candidates).most_common(2)
        bc = default if commons[0][1] == commons[1][1] else commons[idx][0]
        candidates = [c for c in candidates if c[i] == bc]
        i += 1
    return candidates[0]


def part_two():
    report = []
    with open(filename, "r") as f:
        for line in f.readlines():
            num = line.strip()
            report.append(num)

    oxygen = rating(report, "1", 0)
    scrubber = rating(report, "0", 1)
    print(oxygen, scrubber, int(oxygen, 2) * int(scrubber, 2))


# part_one()
part_two()
