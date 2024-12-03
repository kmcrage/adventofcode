use regex::Regex;
use std::fs::read_to_string;

fn calculate(input: &str) -> i32 {
    Regex::new(r"mul\((\d+),(\d+)\)")
        .unwrap()
        .captures_iter(input)
        .map(|caps| caps[1].parse::<i32>().unwrap() * caps[2].parse::<i32>().unwrap())
        .sum()
}

fn main() {
    let file = "./inputs/2024-12-03.txt";
    // let file = "./inputs/test.txt";

    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    println!("part1: {}", calculate(input.as_str()));

    // (?s): . matches \n
    let redacted = Regex::new(r"(?s)don\'t\(\).*?(do\(\)|$)")
        .unwrap()
        .replace_all(input.as_str(), "");
    println!("part2: {}", calculate(&redacted));
}
