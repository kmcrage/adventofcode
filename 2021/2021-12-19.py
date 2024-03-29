#!/usr/bin/python3

from itertools import permutations

filename = "2021-12-19.dat"


def test_match(pos_scanner, scanner_known, beacons_std, beacons_known):
    beacons_std_set = {
        tuple(pos_scanner[i] + test_std[i] for i in [0, 1, 2])
        for test_std in beacons_std
    }

    matches = 0
    for test_known in beacons_known:
        if test_known in beacons_std_set:
            matches += 1

        if matches >= 12:
            print(
                "matched",
                scanner,
                scanner_known,
                pos_scanner,
            )
            scanners_known[scanner] = (pos_scanner, [])
            for b in beacons_std:
                scanners_known[scanner][1].append(
                    tuple(pos_scanner[i] + b[i] for i in [0, 1, 2])
                )
            return True


#
# This could be much faster with tests at every choice of sign
#
def orient_scanner(beacons_known, beacons):
    for orient in permutations([0, 1, 2]):
        for xsgn in [-1, 1]:
            for ysgn in [-1, 1]:
                for zsgn in [-1, 1]:
                    beacons_std = [
                        [
                            xsgn * beacon[orient[0]],
                            ysgn * beacon[orient[1]],
                            zsgn * beacon[orient[2]],
                        ]
                        for beacon in beacons
                    ]
                    for beacon in beacons_std:
                        for known in beacons_known:
                            pos_scanner = [known[i] - beacon[i] for i in [0, 1, 2]]
                            if test_match(
                                pos_scanner,
                                scanner_known,
                                beacons_std,
                                beacons_known,
                            ):
                                return True
    return False


def parse():
    beacons_all = []
    with open(filename, "r") as f:
        for line in f.readlines():
            if "scanner" in line:
                beacons_all.append([])
                continue

            if "," not in line:
                continue
            coords = tuple(int(c) for c in line.strip().split(","))
            beacons_all[-1].append(coords)
    return beacons_all


def get_fingerprints(beacons_all):
    fingerprints = []
    for beacons in beacons_all:
        fingerprints.append(set())
        for a in beacons:
            for b in beacons:
                fingerprint = tuple(sorted(abs(a[i] - b[i]) for i in [0, 1, 2]))
                fingerprints[-1].add(fingerprint)
    return fingerprints


beacons_all = parse()
# fingerprint: sorted abs distances
fingerprints = get_fingerprints(beacons_all)
# scanner: pos, perm, signs
scanners_known = {0: ([0, 0, 0], beacons_all[0])}
tested_pairs = set()
while len(scanners_known) < len(beacons_all):
    for scanner, beacons in enumerate(beacons_all):
        if scanner in scanners_known:
            continue

        for scanner_known, scanner_known_info in scanners_known.items():
            pair = (scanner, scanner_known)
            if pair in tested_pairs:
                continue
            tested_pairs.add(pair)

            # fast tests of magnitude only, no orientation checks
            if (
                len(fingerprints[scanner].intersection(fingerprints[scanner_known]))
                < 66
            ):
                continue

            # slow orientation checks
            if orient_scanner(scanner_known_info[1], beacons):
                break

beacons_all = set()
for scanner, beacons in scanners_known.items():
    for beacon in beacons[1]:
        beacons_all.add(beacon)
print("Part one: num beacons", len(beacons_all))

dist = 0
for scanner_a in [s[0] for s in scanners_known.values()]:
    for scanner_b in [s[0] for s in scanners_known.values()]:
        man_dist = sum(abs(scanner_a[i] - scanner_b[i]) for i in [0, 1, 2])
        dist = max(dist, man_dist)
print("Part two: max dist", dist)
