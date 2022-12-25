#!/usr/bin/python3

DIGITS = {"2": 2, "1": 1, "0": 0, "-": -1, "=": -2}
SDIGITS = {v: k for k, v in DIGITS.items()}

data_filename = "2022-12-25.dat"
# data_filename = "test.dat"


def snafu_to_dec(snafu):
    tokens = list(snafu.strip()[::-1])
    return sum(DIGITS[d] * pow(5, p) for p, d in enumerate(tokens))


def dec_to_snafu(num):
    snafu = ""
    while num:
        digit = num % 5
        num //= 5
        if digit > 2:
            digit -= 5
            num += 1
        snafu += SDIGITS[digit]
    return snafu[::-1]


def part_one(filename):
    total = 0
    with open(filename, "r") as f:
        for line in f:
            num = snafu_to_dec(line)
            # print(line.strip(), num, dec_to_snafu(num))
            total += num
    snafu_total = dec_to_snafu(total)
    print(f"snafu total: {snafu_total}")


part_one(data_filename)
