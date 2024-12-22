use hashbrown::HashMap;
use std::fs::read_to_string;

fn compute(secret: i64) -> i64 {
    let mut result = secret;
    result = ((result * 64) ^ result) % 16777216;
    result = ((result / 32) ^ result) % 16777216;
    result = ((result * 2048) ^ result) % 16777216;
    result
}

fn get_bananas(line: &str) -> HashMap<i64, i64> {
    let seed = line.parse::<i64>().unwrap();
    let mut secrets: Vec<i64> = vec![seed];
    let mut diffs: Vec<i64> = Default::default();
    let mut bananas: HashMap<i64, i64> = Default::default();
    for _ in 0..2000 {
        let next = compute(secrets[secrets.len() - 1]);
        secrets.push(next);
        diffs.push(next % 10 - secrets[secrets.len() - 2] % 10);

        if diffs.len() > 3 {
            // convert to a number since d+10 in 0..19
            let hint = diffs.iter().rev().take(4).copied().fold(0, |a, d| a*20 +(d+10));
            bananas
                .entry(hint)
                .or_insert(secrets[secrets.len() - 1] % 10);
        }
    }
    bananas
}

fn part2(input: &str) -> i64 {
    let mut bananas: HashMap<i64, i64> = Default::default();
    for line in input.lines() {
        let b = get_bananas(line);
        for (hint, &value) in b.iter() {
            bananas
                .entry(*hint)
                .and_modify(|b| *b += value)
                .or_insert(value);
        }
    }

    bananas.values().copied().max().unwrap()
}

fn part1(input: &str) -> i64 {
    input
        .lines()
        .map(|l| String::from(l).parse::<i64>().unwrap())
        .map(|n| (0..2000).fold(n, |a, _| compute(a)))
        .sum::<i64>()
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-22.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    println!("part1: {}", part1(input.as_str()));
    println!("part2: {}", part2(input.as_str()));
}
