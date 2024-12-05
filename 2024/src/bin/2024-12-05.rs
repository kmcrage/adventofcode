use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs::read_to_string;

type Rules = Vec<Vec<usize>>;
struct Update {
    map: HashMap<usize, usize>,
    pages: Vec<usize>,
}

impl Update {
    fn validate(&self, rule: &[usize]) -> bool {
        if let (Some(&ii), Some(&jj)) = (rule.first(), rule.get(1)) {
            if let (Some(&uu), Some(&vv)) = (self.map.get(&ii), self.map.get(&jj)) {
                return uu < vv;
            }
        }
        true
    }

    fn sorted(&self, rules: &Rules) -> Vec<usize> {
        let mut sorted = self.pages.clone();
        sorted.sort_by(|a, b| {
            rules
                .iter()
                .find_map(|rule| {
                    if let (Some(x), Some(y)) = (rule.first(), rule.get(1)) {
                        if x == a && y == b {
                            return Some(Ordering::Less);
                        } else if x == b && y == a {
                            return Some(Ordering::Greater);
                        }
                    }
                    None
                })
                .unwrap_or(Ordering::Equal)
        });
        sorted
    }
}

fn parse(input: String) -> (Rules, Vec<Update>) {
    let (in1, in2) = input.split_once("\n\n").unwrap();

    (
        in1.lines()
            .map(|l| l.split('|').map(|n| n.parse::<usize>().unwrap()).collect())
            .collect(),
        in2.lines()
            .map(|line| {
                let pages: Vec<usize> = line
                    .split(',')
                    .map(|n| n.parse::<usize>().unwrap())
                    .collect();
                Update {
                    map: pages.iter().enumerate().map(|(i, &n)| (n, i)).collect(),
                    pages: pages.clone(),
                }
            })
            .collect(),
    )
}

fn part1(rules: &Rules, updates: &[Update]) -> usize {
    updates
        .iter()
        .filter(|update| rules.iter().all(|rule| update.validate(rule)))
        .map(|update| update.pages[update.pages.len() / 2])
        .sum()
}

fn part2(rules: &Rules, updates: &[Update]) -> usize {
    updates
        .iter()
        .filter(|update| rules.iter().any(|rule| !update.validate(rule)))
        .map(|update| {
            let sorted = update.sorted(rules);
            sorted[sorted.len() / 2]
        })
        .sum()
}

fn main() {
    let file = "./inputs/2024-12-05.txt";
    // let file = "./inputs/test.txt";

    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let (rules, updates) = parse(input);
    println!("part1: {}", part1(&rules, &updates));
    println!("part2: {}", part2(&rules, &updates));
}
