#!/usr/bin/python3

from collections import Counter

filename = "2021-12-08.dat"

digits = [
    "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg",
]


def part_one():
    num_segments = {digit: len(signals) for digit, signals in enumerate(digits)}

    unique_num_segments = {
        v for v, c in Counter(num_segments.values()).most_common() if c == 1
    }

    score = 0
    with open(filename, "r") as f:
        for line in f.readlines():
            signals = line.strip().split("|")[1].strip().split()
            for signal in signals:
                if len(signal) in unique_num_segments:
                    score += 1
    print("score:", score)


def analyze_signals(signals):
    """Break up the digit input into a set of frequency stats."""
    statistics = {}
    for signal in signals:
        for s in list(signal):
            if s not in statistics:
                statistics[s] = {}
            if len(signal) not in statistics[s]:
                statistics[s][len(signal)] = 0
            statistics[s][len(signal)] += 1
    return statistics


def part_two():
    # gather stats on the correct wiring, and generate a lut
    reference = {frozenset(s.items()): d for d, s in analyze_signals(digits).items()}

    result = 0
    with open(filename, "r") as f:
        for line in f.readlines():
            dgts, numbers = [token.strip().split() for token in line.strip().split("|")]

            # gather stats on the faulty wiring...
            statistics = analyze_signals(dgts)

            # ...and translate back into the true labelling...
            translate = {
                signal: reference[frozenset(statistic.items())]
                for signal, statistic in statistics.items()
            }

            # ...use the true labeling to figure out the digits
            digits_lut = {s: str(n) for n, s in enumerate(digits)}
            output = "".join(
                digits_lut["".join(sorted([translate[s] for s in list(num)]))]
                for num in numbers
            )
            result += int(output)
    print(result)


# part_one()
part_two()
