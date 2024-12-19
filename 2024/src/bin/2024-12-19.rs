use hashbrown::HashMap;
use regex::Regex;
use std::fs::read_to_string;

#[derive(Clone, Debug)]
struct Towels {
    towels: Vec<String>,
    patterns: Vec<String>,
    cache: HashMap<String, usize>,
}

impl Towels {
    fn from(input: &str) -> Self {
        let (in_a, in_b) = input.split_once("\n\n").unwrap();

        Towels {
            patterns: in_a.split(", ").map(String::from).collect(),
            cache: HashMap::new(),
            towels: in_b.lines().map(String::from).collect(),
        }
    }

    fn simple_matches(&self) -> usize {
        let regex = format!("^({})*$", self.patterns.join("|"));
        let re = Regex::new(&regex).unwrap();

        self.towels
            .iter()
            .filter(|towel| re.is_match(towel))
            .count()
    }

    fn total_matches(&self) -> usize {
        let mut p = self.clone();
        self.towels
            .iter()
            .map(|towel| p.matches_dp(towel.as_str()))
            .sum()
    }

    fn matches_dp(&mut self, towel: &str) -> usize {
        if towel.is_empty() {
            return 1;
        }
        if let Some(&result) = self.cache.get(towel) {
            return result;
        }

        let mut cnt = 0;
        for pattern in self.patterns.clone() {
            if towel.starts_with(&pattern) {
                cnt += self.matches_dp(&towel[pattern.len()..]);
            }
        }
        self.cache.insert(towel.to_string(), cnt);
        cnt
    }
}

fn main() {
    //let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-19.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let towels = Towels::from(input.as_str());
    println!("part1: {:?}", towels.simple_matches());
    println!("part1: {:?}", towels.total_matches());
}
