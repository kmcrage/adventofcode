// use rayon::prelude::*;
use std::fs::read_to_string;

fn part(input: &str, mode: bool) -> usize {
    input
        .lines()
        .map(|line| {
            let (ans_str, num_str) = line.split_once(": ").unwrap();
            let ans = ans_str.parse::<usize>().unwrap();
            let nums: Vec<usize> = num_str
                .split_whitespace()
                .map(|n| n.parse::<usize>().unwrap())
                .collect();

            if dfs(nums[0], &nums[1..], ans, mode) {
                return ans;
            }
            0
        })
        .sum()
}

fn dfs(num: usize, nums: &[usize], ans: usize, mode: bool) -> bool {
    if nums.is_empty() {
        return num == ans;
    }

    let m = nums[0];
    let mut candidates = vec![num + m, num * m];
    if mode {
        candidates.push(m + num * 10_usize.pow(m.to_string().len() as u32));
    }
    for candidate in candidates {
        if candidate <= ans && dfs(candidate, &nums[1..], ans, mode) {
            return true;
        }
    }
    false
}

fn main() {
    let file = "./inputs/2024-12-07.txt";
    //let file = "./inputs/test.txt";

    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    println!("part1: {}", part(&input, false));
    println!("part2: {}", part(&input, true));
}
