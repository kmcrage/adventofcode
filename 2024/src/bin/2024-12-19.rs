use hashbrown::HashMap;
use regex::Regex;
use std::fs::read_to_string;

fn from(input: &str) -> (Vec<&str>, Vec<&str>) {
    let (in_a, in_b) = input.split_once("\n\n").unwrap();
    let towels = in_b.lines().collect();
    let patterns = in_a.split(", ").collect();

    (towels, patterns)
}

fn simple_matches(towels: &[&str], patterns: &[&str]) -> usize {
    let regex = format!("^({})*$", patterns.join("|"));
    let re = Regex::new(&regex).unwrap();

    towels.iter().filter(|towel| re.is_match(towel)).count()
}

fn total_matches(towels: &[&str], patterns: &[&str]) -> usize {
    let mut cache: HashMap<&str, usize> = HashMap::new();
    towels
        .iter()
        .map(|towel| matches_dp(towel, patterns, &mut cache))
        .sum()
}

fn matches_dp<'a>(towel: &'a str, patterns: &[&str], cache: &mut HashMap<&'a str, usize>) -> usize {
    if towel.is_empty() {
        return 1;
    }
    if let Some(&result) = cache.get(towel) {
        return result;
    }

    let cnt = patterns
        .iter()
        .filter(|&p| towel.starts_with(p))
        .map(|p| matches_dp(&towel[p.len()..], patterns, cache))
        .sum();
    cache.insert(towel, cnt);
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
