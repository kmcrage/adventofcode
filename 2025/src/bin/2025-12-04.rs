use std::fs::read_to_string;

#[derive(Clone, Debug)]
struct Rolls {
    positions: Vec<bool>,
    width: i32,
    height: i32,
}

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
    let height = input.lines().count();
    let rolls = input
        .lines()
        .flat_map(|line| line.chars().map(|c| c == '@').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let width = rolls.len() / height;
    Rolls {
        positions: rolls,
        height: height as i32,
        width: width as i32,
    }
}

fn has_access(map: &Rolls, idx: usize) -> bool {
    DIRS.iter()
        .filter(|(di, dj)| {
            let i = idx as i32 / map.width + di;
            let j = idx as i32 % map.width + dj;
            i >= 0
                && i < map.height
                && j >= 0
                && j < map.width
                && map.positions[i as usize * map.width as usize + j as usize]
        })
        .count()
        < 4
}

fn part1(map: &Rolls) -> usize {
    (0..map.positions.len())
        .filter(|&idx| map.positions[idx] && has_access(map, idx))
        .count()
}

fn part2(map: &Rolls) -> usize {
    let mut removals = 0;
    let rolls: &mut Rolls = &mut map.clone();
    loop {
        let rmvls = (0..rolls.positions.len())
            .filter(|&idx| rolls.positions[idx] && has_access(rolls, idx))
            .collect::<Vec<_>>();
        if rmvls.is_empty() {
            break;
        }
        for &idx in rmvls.iter() {
            rolls.positions[idx] = false;
        }
        removals += rmvls.len();
    }

    removals
}

fn main() {
    let rolls = parse("./inputs/2025-12-04.txt");
    // let rolls = parse("./inputs/example.txt");

    println!("part1: {}", part1(&rolls));
    println!("part2: {}", part2(&rolls));
}
