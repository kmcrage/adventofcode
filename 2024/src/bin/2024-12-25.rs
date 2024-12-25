use std::fs::read_to_string;

fn from(input: &str) -> (Vec<[i32; 5]>, Vec<[i32; 5]>) {
    let mut locks: Vec<[i32; 5]> = Default::default();
    let mut keys: Vec<[i32; 5]> = Default::default();
    input.split("\n\n").for_each(|chunk| {
        let mut is_lock: Option<bool> = None;
        let mut lk: [i32; 5] = [0; 5];
        chunk.lines().enumerate().for_each(|(j, line)| {
            if is_lock.is_none() {
                if line == "#####" {
                    is_lock = Some(true);
                } else {
                    is_lock = Some(false);
                }
            }
            line.chars().enumerate().for_each(|(i, c)| {
                if is_lock.unwrap() && c == '#' {
                    lk[i] = j as i32;
                } else if !is_lock.unwrap() && c == '.' {
                    lk[i] = 5 - j as i32;
                }
            });
        });
        if is_lock.unwrap() {
            locks.push(lk);
        } else {
            keys.push(lk);
        }
    });

    locks.sort();
    keys.sort();
    (locks, keys)
}

fn part1(locks: Vec<[i32; 5]>, keys: Vec<[i32; 5]>) -> usize {
    locks
        .iter()
        .map(|l| {
            keys.iter()
                .filter(|k| k.iter().zip(l.iter()).all(|pair| pair.0 + pair.1 < 6))
                .count()
        })
        .sum()
}
fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-25.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let (locks, keys) = from(input.as_str());
    println!("part1: {}", part1(locks, keys));
}