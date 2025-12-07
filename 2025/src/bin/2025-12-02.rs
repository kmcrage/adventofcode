use std::{
    cmp::{max, min},
    fs::read_to_string,
};

#[derive(Debug)]
struct Pattern {
    repeats: u32,
    digits: u32,
    multiplier: i64,
}

impl Pattern {
    fn length(&self) -> u32{
        self.repeats * self.digits
    }
}

fn parse_range(range: &str) -> (i64, i64) {
    let (from, to) = range.split_once('-').unwrap();

    (from.parse::<i64>().unwrap(), to.parse::<i64>().unwrap())
}

fn repeats(from: i64, to: i64, pattern: &Pattern) -> i64 {
    let mut start = max(
        from / 10_i64.pow(pattern.length() - pattern.digits),
        10_i64.pow(pattern.digits - 1),
    );
    let mut end = min(
        to / 10_i64.pow(pattern.length() - pattern.digits),
        10_i64.pow(pattern.digits) - 1,
    );

    let mut scale = 1;
    (1..pattern.repeats).for_each(|_| {
        scale = scale * 10_i64.pow(pattern.digits) + 1;
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

fn repetitions(splits: &[Pattern], input: &str) -> i64 {
    input
        .split(',')
        .map(|range| {
            let (from, to) = parse_range(range);
            splits.iter().fold(0, |cnt, pattern| {
                cnt + pattern.multiplier * repeats(from, to, pattern)
            })
        })
        .sum()
}

fn max_length(input: &str) -> u32 {
    input
        .split(',')
        .map(|r| r.split('-').next_back().unwrap().len())
        .max()
        .unwrap() as u32
}

fn patterns(max: u32) -> Vec<Pattern> {
    let mut result = Vec::new();
    for length in 2..=max {
        let mut result_len : Vec<Pattern>= Vec::new();
        for digits in (1..length).rev().filter(|&d|length.is_multiple_of(d)) {
            let multiplier = 1 - result_len
                .iter()
                .filter(|p| p.digits.is_multiple_of(digits))
                .map(|p| p.multiplier)
                .sum::<i64>();
            if multiplier != 0 {
                result_len.push(Pattern {
                    repeats: length/digits,
                    digits,
                    multiplier,
                });
            }
        }
        result.extend(result_len);
    }

    result
}

fn part1(input: &str) -> i64 {
    let max_len = max_length(input);
    let patterns: Vec<Pattern> = (2..=max_len)
        .step_by(2)
        .map(|l| Pattern {
            repeats: 2,
            digits: l / 2,
            multiplier: 1,
        })
        .collect();
    repetitions(&patterns, input)
}

fn part2(input: &str) -> i64 {
    let max_len = max_length(input);
    let patterns = patterns(max_len);
    repetitions(&patterns, input)
}

fn main() {
    let file = "./inputs/2025-12-02.txt";
    // let file = "./inputs/example.txt";

    let input = read_to_string(file).unwrap();

    println!("part1: {}", part1(&input));
    println!("part2: {}", part2(&input));
}
