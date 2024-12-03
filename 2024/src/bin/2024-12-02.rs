use itertools::Itertools;
use std::fs::read_to_string;

fn is_nearly_safe(report: &[i32]) -> bool {
    for i in 0..report.len() {
        let mut rep = report.to_vec();
        rep.remove(i);
        if is_safe(&rep) {
            return true;
        }
    }
    false
}

fn is_safe(report: &[i32]) -> bool {
    let mut safe = false;

    safe |= report.iter().tuple_windows().all(|(l, r)| l > r);
    safe |= report.iter().tuple_windows().all(|(l, r)| l < r);

    safe && report
        .iter()
        .tuple_windows()
        .all(|(&l, &r)| (1..=3).contains(&l.abs_diff(r)))
}

fn main() {
    let file = "./inputs/2024-12-02.txt";
    //let file = "./inputs/test.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let parsed: Vec<Vec<i32>> = input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|n| n.parse::<i32>().unwrap())
                .collect()
        })
        .collect();

    let part1 = parsed.iter().filter(|report| is_safe(report)).count();
    println!("part1: {part1}");

    let part2 = parsed
        .iter()
        .filter(|report| is_nearly_safe(report))
        .count();
    println!("part2: {part2}");
}
