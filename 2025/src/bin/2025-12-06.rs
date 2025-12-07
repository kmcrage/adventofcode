use std::fs::read_to_string;

fn part1(input: &str) -> i64 {
    let cols: Vec<Vec<&str>> = input
        .lines()
        .map(|line| line.split_whitespace().collect())
        .collect();

    let nums: Vec<Vec<i64>> = (0..cols[0].len())
        .map(|i| {
            cols.iter()
                .take(cols.len() - 1)
                .map(|row| row[i].parse::<i64>().unwrap())
                .collect()
        })
        .collect();

    cols[cols.len() - 1]
        .iter()
        .enumerate()
        .map(|(i, op)| match op {
            &"*" => nums[i].iter().product::<i64>(),
            _ => nums[i].iter().sum::<i64>(),
        })
        .sum()
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

    let ops: Vec<_> = input
        .lines()
        .next_back()
        .unwrap()
        .split_whitespace()
        .collect();
    ops.iter()
        .enumerate()
        .map(|(i, op)| match op {
            &"*" => vnums[i].iter().product::<i64>(),
            _ => vnums[i].iter().sum::<i64>(),
        })
        .sum::<i64>()
}

fn main() {
    let input = read_to_string("./inputs/2025-12-06.txt").unwrap();
    // let input = read_to_string("./inputs/example.txt").unwrap();

    println!("part1: {}", part1(&input));
    println!("part2: {}", part2(&input));
}
