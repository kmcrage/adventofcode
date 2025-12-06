use std::fs::read_to_string;

fn part1(input: &str) -> i64 {
    let cols: Vec<Vec<&str>> = input
        .lines()
        .map(|line| line.split_whitespace().collect())
        .collect();

    let mut nums = Vec::new();
    for i in 0..cols[0].len() {
        let mut ns: Vec<i64> = Vec::new();
        for row in cols.iter().take(cols.len() - 1) {
            let num = row[i].parse::<i64>().unwrap();
            ns.push(num);
        }
        nums.push(ns);
    }

    let mut sum = 0;
    for i in 0..cols[0].len() {
        let op = cols[cols.len() - 1][i];
        if op == "*" {
            sum += nums[i].iter().product::<i64>();
        } else {
            sum += nums[i].iter().sum::<i64>();
        }
    }
    sum
}

fn part2(input: &str) -> i64 {
    let data: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    let mut vnums: Vec<Vec<i64>> = Vec::new();
    let mut nums: Vec<i64> = Vec::new();
    for i in 0..data[0].len() {
        let mut num = 0;
        let mut blank = true;
        for row in data.iter().take(data.len() - 1) {
            if row[i] != ' ' {
                blank = false;
            }
            if let Some(d) = row[i].to_digit(10) {
                num = num * 10 + d as i64;
            }
        }

        let last_column = i == data[0].len() - 1;
        if !blank || last_column {
            nums.push(num);
        }
        if blank || last_column {
            vnums.push(nums.clone());
            nums.clear();
        }
    }

    let mut sum = 0;
    let ops: Vec<_> = input
        .lines()
        .next_back()
        .unwrap()
        .split_whitespace()
        .collect();
    for i in 0..ops.len() {
        let op = ops[i];
        if op == "*" {
            sum += vnums[i].iter().product::<i64>();
        } else {
            sum += vnums[i].iter().sum::<i64>();
        }
    }
    sum
}

fn main() {
    let input = read_to_string("./inputs/2025-12-06.txt").unwrap();
    // let input = read_to_string("./inputs/example.txt").unwrap();

    println!("part1: {}", part1(&input));
    println!("part2: {}", part2(&input));
}
