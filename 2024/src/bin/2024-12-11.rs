use hashbrown::HashMap;
use num::pow;
use std::fs::read_to_string;

type Stones = HashMap<usize, usize>;

fn to_stones(input: &str) -> Stones {
    let mut stones: HashMap<usize, usize> = Default::default();

    for s in input.split_whitespace() {
        let stone = s.to_string().parse::<usize>().unwrap();
        stones
            .entry(stone)
            .and_modify(|c| {
                *c += 1;
            })
            .or_insert(1);
    }
    stones
}

fn updated(stone: usize) -> Vec<usize> {
    let mut updates: Vec<usize> = Default::default();
    if stone == 0 {
        updates.push(1);
    } else {
        let digits = stone.ilog10() as usize + 1;
        if digits % 2 == 0 {
            let md = pow(10, digits / 2);
            updates.push(stone / md);
            updates.push(stone % md);
        } else {
            updates.push(stone * 2024);
        }
    }
    updates
}


fn blink(input: &Stones) -> Stones {
    let mut stones: Stones = Default::default();
    for (&stone, &num) in input.iter() {
        for update in updated(stone) {
            stones
                .entry(update)
                .and_modify(|c| {
                    *c += num;
                })
                .or_insert(num);
        }
    }
    stones
}

fn blinks(input: &Stones, num: usize) -> usize {
    let mut stones = input.clone();
    for _ in 0..num {
        stones = blink(&stones)
    }
    stones.values().sum()
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-11.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let stones = to_stones(input.as_str());
    println!("part1: {:?}", blinks(&stones, 25));
    println!("part2: {:?}", blinks(&stones, 75));
}
