use std::fs::read_to_string;

fn compute(secret: i64) -> i64 {
    let mut result = secret;
    result = ((result * 64) ^ result) % 16777216;
    result = ((result / 32) ^ result) % 16777216;
    result = ((result * 2048) ^ result) % 16777216;
    result
}

fn get_bananas(line: &str, bananas: &mut [i64]) {
    let seed = line.parse::<i64>().unwrap();
    let mut secrets = (seed, seed);
    let mut seen = [false; 160_000];
    let mut hint = 0_usize;
    for i in 0..2000 {
        secrets = (compute(secrets.0), secrets.0);
        let diff =  secrets.0 % 10 - secrets.1 % 10;
        hint = hint / 20 + 8000 * (10 + diff) as usize; // 8000 == 20 ** 3
        if i > 3 && !seen[hint] {
            bananas[hint] += secrets.0 % 10;
            seen[hint] = true;
        }
    }
}

fn part2(input: &str) -> i64 {
    let mut bananas: [i64; 160000] = [0; 160_000];
    input.lines().for_each(|line| {get_bananas(line, &mut bananas)});
    bananas.iter().copied().max().unwrap()
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
