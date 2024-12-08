// use rayon::prelude::*;
use std::fs::read_to_string;

fn part(input: &str, mode: bool) -> isize {
    input
        .lines()
        .map(|line| {
            let (ans_str, num_str) = line.split_once(": ").unwrap();
            let ans = ans_str.parse::<isize>().unwrap();
            let nums: Vec<isize> = num_str
                .split_whitespace()
                .map(|n| n.parse::<isize>().unwrap())
                .collect();

            if dfs(ans, &nums, mode) {
                return ans;
            }
            0
        })
        .sum()
}

fn dfs(ans: isize, nums: &[isize], mode: bool) -> bool {
    if nums.len() == 1 {
        return nums[0] == ans;
    }

    let num = nums[nums.len() - 1];
    if num < ans && dfs(ans - num, &nums[..nums.len() - 1], mode) {
        return true;
    }
    if ans % num == 0 && dfs(ans / num, &nums[..nums.len() - 1], mode) {
        return true;
    }
    if mode {
        let exp = 10_isize.pow(num.to_string().len() as u32);
        if ans % exp == num && dfs(ans / exp, &nums[..nums.len() - 1], mode) {
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
