// use rayon::prelude::*;
use std::collections::VecDeque;
use std::fs::read_to_string;

fn part(input: &str, mode: bool) -> usize {
    input
        .lines()
        .map(|line| {
            let (ans_str, num_str) = line.split_once(": ").unwrap();
            let ans = ans_str.parse::<usize>().unwrap();
            let mut nums: VecDeque<usize> = num_str
                .split_whitespace()
                .map(|n| n.parse::<usize>().unwrap())
                .collect();

            let mut results: VecDeque<usize> = Default::default();
            results.push_back(nums.pop_front().unwrap());
            while !nums.is_empty() {
                let mut results_next: VecDeque<usize> = Default::default();
                let n = nums.pop_front().unwrap();
                for m in results {
                    let mut candidates = vec![m + n, m * n];
                    if mode {
                        candidates.push(format!("{m}{n}").parse::<usize>().unwrap());
                    }
                    for candidate in candidates {
                        if candidate <= ans {
                            results_next.push_back(candidate);
                        }
                    }
                }
                results = results_next;
            }
            if results.contains(&ans) {
                return ans;
            }
            0
        })
        .sum()
}

fn main() {
    let file = "./inputs/2024-12-07.txt";
    //let file = "./inputs/test.txt";

    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    println!("part1: {}", part(&input, false));
    println!("part2: {}", part(&input, true));
}
