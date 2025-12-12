use hashbrown::{HashMap, HashSet};
use std::fs::read_to_string;

// note that all shapes are 3x3

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Problem {
    width: usize,
    height: usize,
    counts: Vec<usize>,
    board: Vec<bool>,
}
type Shape = Vec<Vec<bool>>;
type Cache = HashMap<(Vec<usize>, Vec<bool>), bool>;

fn parse(input: &str) -> (Vec<Shape>, Vec<Problem>) {
    let sections = input.split("\n\n").collect::<Vec<_>>();
    let mut shapes = Vec::new();
    for section in sections[0..sections.len() - 1].iter() {
        let mut shape = section
            .lines()
            .skip(1)
            .flat_map(|l| l.chars().map(|c| c == '#'))
            .collect::<Vec<_>>();
        let mut variants = HashSet::new();
        for _ in 0..4 {
            variants.insert(shape.clone());
            let flipped = flip(&shape);
            variants.insert(flipped);
            shape = rotate(&shape);
        }
        shapes.push(variants.clone().into_iter().collect());
    }

    let problems = sections
        .iter()
        .next_back()
        .unwrap()
        .lines()
        .map(|line| {
            let (size, counts) = line.split_once(": ").unwrap();
            let size = size.split_once('x').unwrap();
            let width = size.0.parse::<usize>().unwrap();
            let height = size.1.parse::<usize>().unwrap();
            Problem {
                width,
                height,
                counts: counts
                    .split_whitespace()
                    .map(|c| c.parse::<usize>().unwrap())
                    .collect(),
                board: vec![false; width * height],
            }
        })
        .collect();

    (shapes, problems)
}

fn flip(shape: &[bool]) -> Vec<bool> {
    let mut out = vec![false; 9];
    for i in 0..3 {
        for j in 0..3 {
            out[i + 3 * j] = shape[j + 3 * i]
        }
    }
    out
}

fn rotate(shape: &[bool]) -> Vec<bool> {
    let mut out = vec![false; 9];
    for i in 0..3 {
        for j in 0..3 {
            out[i + 3 * j] = shape[j + 3 * (2 - i)]
        }
    }
    out
}

fn fit_test(i: usize, j: usize, shape: &[bool], problem: &Problem) -> bool {
    for ii in 0..3 {
        for jj in 0..3 {
            if shape[ii + 3 * jj] && problem.board[i + ii + problem.width * (j + jj)] {
                return false;
            }
        }
    }
    true
}

fn fit(i: usize, j: usize, shape: &[bool], problem: &Problem) -> Vec<bool> {
    let mut result = problem.board.to_vec();
    for ii in 0..3 {
        for jj in 0..3 {
            if shape[ii + 3 * jj] {
                result[i + ii + problem.width * (j + jj)] = true;
            }
        }
    }
    result
}

fn solve(problem: &Problem, shapes: &[Shape], cache: &mut Cache) -> bool {
    // println!("solve {:?}", problem);
    if problem.counts.iter().all(|&c| c == 0) {
        // println!("fits");
        cache.insert((problem.counts.clone(), problem.board.clone()), true);
        return true;
    }
    if cache.contains_key(&(problem.counts.clone(), problem.board.clone())) {
        return cache[&(problem.counts.clone(), problem.board.clone())];
    }

    let shape_num = problem
        .counts
        .iter()
        .enumerate()
        .find(|c| *c.1 > 0)
        .unwrap()
        .0;
    for i in 0..(problem.width - 2) {
        for j in 0..(problem.height - 2) {
            for shape in shapes[shape_num].iter() {
                if fit_test(i, j, shape, problem) {
                    let mut next = problem.clone();
                    next.counts[shape_num] -= 1;
                    next.board = fit(i, j, shape, problem);
                    // println!("    next {:?} {shape_num} {:?}", problem.counts, next);
                    if solve(&next, shapes, cache) {
                        cache.insert((next.counts.clone(), next.board.clone()), true);
                        return true;
                    }
                }
            }
        }
    }
    // println!("failed");
    cache.insert((problem.counts.clone(), problem.board.clone()), false);
    false
}

fn part1(shapes: &[Shape], problems: &[Problem]) -> usize {
    problems
        .iter()
        .filter(|p| {
            let mut cache: Cache = HashMap::new();
            solve(p, shapes, &mut cache)
        })
        .count()
}

fn main() {
    let input = read_to_string("./inputs/example.txt").unwrap();
    // let input = read_to_string("./inputs/2025-12-12.txt").unwrap();
    let (shapes, problems) = parse(&input);

    println!("part1: {:?}", part1(&shapes, &problems));
    //println!("part2: {:?}", combi("svr", "out", &input, true));
}
