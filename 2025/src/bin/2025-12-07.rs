use hashbrown::{HashMap, HashSet};
use std::fs::read_to_string;

fn part1(splitters: &HashSet<(i32, i32)>) -> usize {
    let i_max = *splitters.iter().map(|(i, _)| i).max().unwrap();
    let start = *splitters.iter().find(|(i, _)| *i == 0).unwrap();
    let mut queue = vec![start];
    let mut seen: HashMap<(i32, i32), bool> = HashMap::new();
    while let Some(pos) = queue.pop() {
        if pos.0 > i_max || seen.contains_key(&pos) {
            continue;
        }

        if splitters.contains(&pos) {
            seen.insert(pos, true);
            queue.push((pos.0, pos.1 - 1));
            queue.push((pos.0, pos.1 + 1));
        } else {
            seen.insert(pos, false);
            queue.push((pos.0 + 1, pos.1));
        }
    }

    seen.iter().filter(|(_, v)| **v).count()
}

fn num_paths(
    pos: (i32, i32),
    i_max: i32,
    splitters: &HashSet<(i32, i32)>,
    cache: &mut HashMap<(i32, i32), i64>,
) -> i64 {
    if cache.contains_key(&pos) {
        return cache[&pos];
    }

    if pos.0 > i_max {
        return 1;
    }

    let result = match splitters.contains(&pos) {
        true => {
            num_paths((pos.0, pos.1 + 1), i_max, splitters, cache)
                + num_paths((pos.0, pos.1 - 1), i_max, splitters, cache)
        }
        false => num_paths((pos.0 + 1, pos.1), i_max, splitters, cache),
    };
    cache.insert(pos, result);
    result
}

fn part2(splitters: &HashSet<(i32, i32)>) -> i64 {
    let i_max = *splitters.iter().map(|(i, _)| i).max().unwrap();
    let start = *splitters.iter().find(|(i, _)| *i == 0).unwrap();

    let mut cache = HashMap::new();
    num_paths(start, i_max, splitters, &mut cache)
}

fn parse(input: &str) -> HashSet<(i32, i32)> {
    input
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, c)| c != &'.')
                .map(move |(j, _)| (i as i32, j as i32))
        })
        .collect()
}

fn combi(input: &str) -> (i64, i64) {
    let start: Vec<i64> = input
        .lines()
        .next()
        .unwrap()
        .chars()
        .map(|c| if c == 'S' { 1 } else { 0 })
        .collect();
    let num_cols = start.len();

    // (number of splitters, final state of the beams)
    let result = input
        .lines()
        .skip(1)
        .fold((0, start), |(mut num_splitters, beams), line| {
            let mut next = vec![0_i64; num_cols];
            line.chars()
                .zip(beams)
                .enumerate()
                .for_each(|(i, (chr, beam))| {
                    if beam == 0 {
                        return;
                    }
                    match chr {
                        '.' => next[i] += beam,
                        '^' => {
                            next[i - 1] += beam;
                            next[i + 1] += beam;
                            num_splitters += 1;
                        }
                        _ => panic!("bad char at {i}: {chr}"),
                    }
                });
            (num_splitters, next)
        });

    (result.0, result.1.iter().sum())
}

fn main() {
    let input = read_to_string("./inputs/2025-12-07.txt").unwrap();
    //let input = read_to_string("./inputs/example.txt").unwrap();

    let splitters = parse(&input);
    println!("part1: {}", part1(&splitters));
    println!("part2: {}", part2(&splitters));
    println!("combi: {:?}", combi(&input));
}
