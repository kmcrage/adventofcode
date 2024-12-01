use scan_rules::*;

// const INPUT: &str = include_str!("../../inputs/test.txt");
const INPUT: &str = include_str!("../../inputs/2024-12-01.txt");

fn main() {
    let (mut x, mut y): (Vec<_>, Vec<_>) = INPUT
        .lines()
        .map(|line| {
            let_scan!(line; (let a:usize, " ", let b:usize));
            (a, b)
        })
        .unzip();
    x.sort();
    y.sort();

    let part1: usize = x.iter().zip(y.iter()).map(|(&a, &b)| a.abs_diff(b)).sum();

    println!("part1: {}", part1);

    let part2: usize = x
        .iter()
        .map(|&a| a * y.iter().filter(|&n| *n == a).count())
        .sum();
    println!("part2: {}", part2);
}
