use std::collections::HashMap;
use std::fs::read_to_string;

type WordSearch = HashMap<(i32, i32), char>;

fn parse(input: String) -> WordSearch {
    let mut wordsearch: WordSearch = WordSearch::new();

    for (i, line) in input.lines().enumerate() {
        for (j, chr) in line.chars().enumerate() {
            wordsearch.insert((i as i32, j as i32), chr);
        }
    }
    wordsearch
}

fn count_xmas(wordsearch: &WordSearch, word: &str) -> usize {
    let dirs: [(i32, i32); 8] = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    let first = word.chars().next().unwrap();

    wordsearch
        .iter()
        .filter(|(_, &x)| x == first)
        .map(|(&(i, j), _)| {
            dirs.iter()
                .filter(|(di, dj)| {
                    word.chars().enumerate().all(|(k, c)| {
                        let kk = k as i32;
                        Some(&c) == wordsearch.get(&(i + kk * di, j + kk * dj))
                    })
                })
                .count()
        })
        .sum()
}

fn count_x_mas(wordsearch: &WordSearch, word: &str) -> usize {
    wordsearch
        .keys()
        .filter(|&(i, j)| {
            (word.chars().enumerate().all(|(k, c)| {
                let kk = k as i32 - 1;
                Some(&c) == wordsearch.get(&(i + kk, j + kk))
            }) || word.chars().enumerate().all(|(k, c)| {
                let kk = k as i32 - 1;
                Some(&c) == wordsearch.get(&(i - kk, j - kk))
            })) && (word.chars().enumerate().all(|(k, c)| {
                let kk = k as i32 - 1;
                Some(&c) == wordsearch.get(&(i + kk, j - kk))
            }) || word.chars().enumerate().all(|(k, c)| {
                let kk = k as i32 - 1;
                Some(&c) == wordsearch.get(&(i - kk, j + kk))
            }))
        })
        .count()
}

fn main() {
    let file = "./inputs/2024-12-04.txt";
    // let file = "./inputs/test.txt";

    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let wordsearch = parse(input);
    println!("part1: {}", count_xmas(&wordsearch, "XMAS"));
    println!("part2: {}", count_x_mas(&wordsearch, "MAS"));
}
