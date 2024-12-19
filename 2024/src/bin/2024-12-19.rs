use hashbrown::HashMap;
use regex::Regex;
use std::fs::read_to_string;

fn from(input: &str) -> (Vec<String>, Vec<String>) {
    let (in_a, in_b) = input.split_once("\n\n").unwrap();

    (
        in_b.lines().map(String::from).collect(),
        in_a.split(", ").map(String::from).collect(),
    )
}

fn simple_matches(towels: &[String], patterns: &[String]) -> usize {
    let regex = format!("^({})*$", patterns.join("|"));
    let re = Regex::new(&regex).unwrap();

    towels.iter().filter(|towel| re.is_match(towel)).count()
}

fn total_matches(towels: &[String], patterns: &[String]) -> usize {
    let mut cache: HashMap<String, usize> = HashMap::new();
    towels
        .iter()
        .map(|towel| matches_dp(towel, patterns, &mut cache))
        .sum()
}

fn matches_dp(towel: &str, patterns: &[String], cache: &mut HashMap<String, usize>) -> usize {
    if towel.is_empty() {
        return 1;
    }
    if let Some(&result) = cache.get(towel) {
        return result;
    }

    let mut cnt = 0;
    for pattern in patterns {
        if towel.starts_with(pattern) {
            cnt += matches_dp(&towel[pattern.len()..], patterns, cache);
        }
    }
    cache.insert(towel.to_string(), cnt);
    cnt
}

fn main() {
    //let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-19.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let (towels, patterns) = from(input.as_str());
    println!("part1: {:?}", simple_matches(&towels, &patterns));
    println!("part1: {:?}", total_matches(&towels, &patterns));
}
