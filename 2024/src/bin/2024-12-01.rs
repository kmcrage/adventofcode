use std::collections::HashMap;
use std::fs::read_to_string;

fn parse(file: &str) -> (Vec<usize>, Vec<usize>, HashMap<usize, usize>) {
    let input = read_to_string(file).unwrap();

    let mut freq: HashMap<usize, usize> = HashMap::new();
    let (mut left, mut right): (Vec<_>, Vec<_>) = input
        .lines()
        .map(|line| {
            let mut tokens = line.split_whitespace();
            let l = tokens.next().unwrap().parse::<usize>().unwrap();
            let r = tokens.next().unwrap().parse::<usize>().unwrap();
            *freq.entry(r).or_default() += 1;
            (l, r)
        })
        .unzip();

    left.sort();
    right.sort();

    (left, right, freq)
}

fn main() {
    let file = "./inputs/2024-12-01.txt";
    let (left, right, freq) = parse(file);

    let part1: usize = left
        .iter()
        .zip(right.iter())
        .map(|(&l, &r)| l.abs_diff(r))
        .sum();
    println!("part1: {}", part1);

    let part2: usize = left.iter().map(|l| l * freq.get(l).unwrap_or(&0)).sum();
    println!("part2: {}", part2);
}
