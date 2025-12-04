use hashbrown::HashSet;
use std::fs::read_to_string;

type Rolls = HashSet<(i32, i32)>;

const DIRS: [(i32, i32); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn parse(file: &str) -> Rolls {
    let input = read_to_string(file).unwrap();

    let mut rolls = HashSet::new();
    for (i, line) in input.lines().enumerate() {
        for (j, chr) in line.chars().enumerate() {
            if chr == '@' {
                rolls.insert((i as i32, j as i32));
            }
        }
    }
    rolls
}

fn removal(input: &Rolls) -> (usize, Rolls) {
    let mut result = input.clone();
    result.retain(|(i, j)| {
        DIRS.iter()
            .filter(|(x, y)| input.contains(&(i + x, j + y)))
            .count()
            >= 4
    });

    (input.len() - result.len(), result)
}

fn neighbours(map: &Rolls) -> usize {
    removal(map).0
}

fn removals(map: &Rolls) -> usize {
    let mut current = map.clone();
    let mut cnt = 0;
    let mut modified = 1;
    while modified != 0 {
        (modified, current) = removal(&current);
        cnt += modified;
    }

    cnt
}

fn main() {
    let map = parse("./inputs/2025-12-04.txt");
    // let map = parse("./inputs/example.txt");

    let part1 = neighbours(&map);
    println!("part1: {part1}");

    let part2 = removals(&map);
    println!("part2: {part2}");
}
