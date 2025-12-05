use std::{cmp::max, fs::read_to_string};

type Range = (u64, u64);

fn parse(file: &str) -> (Vec<Range>, Vec<u64>) {
    let input = read_to_string(file).unwrap();

    let (ranges, items) = input.split_once("\n\n").unwrap();
    let ranges = ranges
        .lines()
        .map(|l| {
            let (s, e) = l.split_once('-').unwrap();
            (s.parse::<u64>().unwrap(), e.parse::<u64>().unwrap())
        })
        .collect();
    let items = items.lines().map(|l| l.parse::<u64>().unwrap()).collect();
    (ranges, items)
}

fn fresh_items(ranges: &[Range], items: &[u64]) -> usize {
    items
        .iter()
        .filter(|i| ranges.iter().any(|(start, end)| start <= i && end >= i))
        .count()
}

fn merge_overlap(intervals: &[Range]) -> Vec<Range> {
    let mut ranges: Vec<Range> = intervals.to_vec();
    ranges.sort();

    let mut result = vec![ranges[0]];
    ranges.iter().skip(1).for_each(|&range| {
        let l = result.len() - 1;
        if range.0 <= result[l].1 {
            result[l].1 = max(result[l].1, range.1);
        } else {
            result.push(range);
        }
    });
    result
}

fn fresh_ingredients(ranges: &[Range]) -> i64 {
    merge_overlap(ranges)
        .iter()
        .map(|(start, end)| (end - start + 1) as i64)
        .sum()
}

fn main() {
    //let (ranges, items) = parse("./inputs/example.txt");
    let (ranges, items) = parse("./inputs/2025-12-05.txt");

    let part1 = fresh_items(&ranges, &items);
    println!("part1: {part1}");
    let part2 = fresh_ingredients(&ranges);
    println!("part2: {part2}");
}
