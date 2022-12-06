#!/usr/bin/python3

data_filename = "2022-12-06.dat"
# data_filename = "test.dat"


def processed(filename, marker):
    result = None
    with open(filename, "r") as f:
        for line in f:
            for i in range(len(line)):
                if len(set(line[i : i + marker])) == marker:
                    result = i + marker
                    break
            print(f"signal length {marker}, processed {result}")


processed(data_filename, 4)
processed(data_filename, 14)
