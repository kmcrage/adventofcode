#!/usr/bin/python3
import functools

data_filename = "2022-12-13.dat"
# data_filename = "test.dat"


def compare(packet_left, packet_right):
    # print('cmp', packet_left, packet_right)
    for left, right in zip(packet_left, packet_right):
        # print("cmp", left, right)
        if isinstance(left, int) and isinstance(right, int):
            if left != right:
                return 1 if left < right else -1
        elif isinstance(left, list) and isinstance(right, list):
            result = compare(left, right)
            if result is not None:
                return result
        elif isinstance(left, list):
            result = compare(left, [right])
            if result is not None:
                return result
        else:
            result = compare([left], right)
            if result is not None:
                return result
    if len(packet_left) != len(packet_right):
        return 1 if len(packet_left) < len(packet_right) else -1


def part_one(filename):
    result = 0
    packets = []
    with open(filename, "r") as f:
        pair_num = 0
        for line in f:
            line = line.strip()
            if not line:
                continue

            packets.append(eval(line))
            if len(packets) != 2:
                continue
            
            pair_num += 1
            if compare(packets[0], packets[1]) != -1:
                # print("correct", pair_num)
                result += pair_num
            packets = []

    print("part_one:", result)


def part_two(filename):
    packets = [[[2]], [[6]]]
    signals = packets[:]
    with open(filename, "r") as f:
        for line in f:
            line = line.strip()
            if line:
                packets.append(eval(line))

    packets.sort(key=functools.cmp_to_key(compare), reverse=True)

    product = 1
    for p, packet in enumerate(packets, 1):
        if packet in signals:
            product *= p
    print(f"product: {product}")


part_one(data_filename)
part_two(data_filename)
