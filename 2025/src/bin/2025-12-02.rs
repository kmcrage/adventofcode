use std::{
    cmp::{max, min},
    fs::read_to_string,
};

type Split = (u32, u32); // (#digits in target number, #digits in repeated substring)

// repeats x2; other repeats; duplicates (the biggest numbers are ten digits)
const DOUBLES: [Split; 5] = [(2, 1), (4, 2), (6, 3), (8, 4), (10, 5)];
const REPEATS: [Split; 6] = [(3, 1), (5, 1), (6, 2), (7, 1), (9, 3), (10, 2)];
const DUPLICATES: [Split; 2] = [(6, 1), (10, 1)];

fn parse_range(range: &str) -> (i64, i64) {
    let (from, to) = range.split_once('-').unwrap();

    (from.parse::<i64>().unwrap(), to.parse::<i64>().unwrap())
}

fn repeats(from: i64, to: i64, len: u32, digits: u32) -> i64 {
    let mut start = max(from / 10_i64.pow(len - digits), 10_i64.pow(digits - 1));
    let mut end = min(to / 10_i64.pow(len - digits), 10_i64.pow(digits) - 1);

    let mut scale = 1;
    (1..(len / digits)).for_each(|_| {
        scale = scale * 10_i64.pow(digits) + 1;
    });

    if start * scale < from {
        start += 1;
    }
    if end * scale > to {
        end -= 1;
    }
    // sanity check
    if start > end || from > start * scale || end * scale > to {
        return 0;
    }
    (end * (end + 1) - start * (start - 1)) * scale / 2
}

fn repetitions(splits: &[Split], input: &str) -> i64 {
    input
        .split(',')
        .map(|range| {
            let (from, to) = parse_range(range);
            splits
                .iter()
                .fold(0, |cnt, (len, dgts)| cnt + repeats(from, to, *len, *dgts))
        })
        .sum()
}

fn main() {
    let file = "./inputs/2025-12-02.txt";
    // let file = "./inputs/example.txt";

    let input = read_to_string(file).unwrap();

    let part1: i64 = repetitions(&DOUBLES, &input);
    println!("part1: {part1}");

    let part2: i64 = part1 + repetitions(&REPEATS, &input) - repetitions(&DUPLICATES, &input);
    println!("part2: {part2}");
}
