use hashbrown::HashMap;
use std::cmp::min;
use std::fs::read_to_string;
use rayon::iter::{ParallelBridge, ParallelIterator};

type CacheTP = HashMap<Vec<bool>, Vec<(usize, Vec<usize>)>>;
type CacheJ = HashMap<Vec<usize>, usize>;

fn parse(input: &str) -> (Vec<bool>, Vec<Vec<usize>>, Vec<usize>) {
    let tokens = input.split_whitespace().collect::<Vec<_>>();
    let goal = tokens[0]
        .chars()
        .filter(|c| c != &'[' && c != &']')
        .map(|c| matches!(c, '#'))
        .collect();

    let buttons = tokens[1..(tokens.len() - 1)]
        .iter()
        .map(|button| {
            button[1..button.len() - 1]
                .split(',')
                .map(|b| b.parse::<usize>().unwrap())
                .collect()
        })
        .collect();

    let joltage = tokens[tokens.len() - 1];
    let joltage = joltage[1..joltage.len() - 1]
        .split(',')
        .map(|j| j.parse().unwrap())
        .collect();

    (goal, buttons, joltage)
}

fn part1(input: &str) -> usize {
    input
        .lines()
        .par_bridge()
        .map(|line| {
            let (target, buttons, _) = parse(line);
            let mut min_cost = usize::MAX;
            for sol in 1..2_usize.pow(buttons.len() as u32) {
                if sol.count_ones() as usize >= min_cost {
                    continue;
                }

                let mut soln = sol;
                let mut lights = (0..target.len()).map(|_| false).collect::<Vec<_>>();
                for button in &buttons {
                    if !soln.is_multiple_of(2) {
                        for &light in button.iter() {
                            lights[light] = !lights[light];
                        }
                    }
                    soln /= 2;
                }
                if lights == target {
                    min_cost = min(min_cost, sol.count_ones() as usize);
                }
            }
            min_cost
        })
        .sum()
}

fn target_presses(
    target: &[bool],
    buttons: &[Vec<usize>],
    cache: &mut CacheTP,
) -> Vec<(usize, Vec<usize>)> {
    let key = target.to_vec();
    if cache.contains_key(&key) {
        return cache[&key].clone();
    }

    let mut result = Vec::new();
    for sol in 0..2_usize.pow(buttons.len() as u32) {
        let mut soln = sol;
        let mut presses = (0..target.len()).map(|_| 0_usize).collect::<Vec<_>>();
        for button in buttons {
            if !soln.is_multiple_of(2) {
                for &light in button.iter() {
                    presses[light] += 1;
                }
            }
            soln /= 2;
        }
        if presses
            .iter()
            .map(|j| !j.is_multiple_of(2))
            .zip(target)
            .any(|(j, t)| j != *t)
        {
            continue;
        }
        result.push((sol.count_ones() as usize, presses));
    }
    cache.insert(key, result.clone());
    result
}

fn solve_recursive(
    buttons: &[Vec<usize>],
    joltages: &[usize],
    cache_tp: &mut CacheTP,
    cache: &mut CacheJ,
) -> usize {
    let key = joltages.to_vec();
    if cache.contains_key(&key) {
        return cache[&key];
    }
    if joltages.iter().all(|j| *j == 0) {
        return 0;
    }

    let target = joltages
        .iter()
        .map(|j| !j.is_multiple_of(2))
        .collect::<Vec<_>>();
    let mut min_cost = usize::MAX / 3;
    for (cst, presses) in target_presses(&target, buttons, cache_tp) {
        if presses.iter().zip(joltages).any(|(a, b)| a > b) {
            continue;
        }

        let next_joltages = joltages
            .iter()
            .zip(presses)
            .map(|(j, d)| (j - d) / 2)
            .collect::<Vec<_>>();
        let cost = cst + 2 * solve_recursive(buttons, &next_joltages, cache_tp, cache);
        min_cost = min(min_cost, cost);
    }
    cache.insert(key, min_cost);
    min_cost
}

fn part2(input: &str) -> usize {
    input
        .lines()
        .par_bridge()
        .map(|line| {
            let (_, buttons, joltages) = parse(line);
            let mut cache_tp = HashMap::new();
            let mut cache_j = HashMap::new();
            solve_recursive(&buttons, &joltages, &mut cache_tp, &mut cache_j)
        })
        .sum()
}

fn main() {
    // let input = read_to_string("./inputs/example.txt").unwrap();
    let input = read_to_string("./inputs/2025-12-10.txt").unwrap();

    println!("part1: {:?}", part1(&input));
    println!("part2: {:?}", part2(&input));
}
