use good_lp::{highs, Expression, IntoAffineExpression, Solution, SolverModel, variable, variables};
use hashbrown::HashSet;
use std::cmp::min;
use std::collections::VecDeque;
use std::fs::read_to_string;

#[derive(Clone, Debug)]
struct State {
    lights: Vec<bool>,
    depth: usize,
}

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

fn bfs(start: State, target: &[bool], buttons: &[Vec<usize>]) -> usize {
    let mut queue = VecDeque::from([start]);
    let mut seen = HashSet::new();

    while let Some(state) = queue.pop_front() {
        if seen.contains(&state.lights) {
            continue;
        }
        seen.insert(state.lights.clone());
        for presses in buttons {
            let next = &mut state.clone();
            next.depth += 1;
            for &press in presses {
                next.lights[press] = !next.lights[press];
            }
            if next.lights == target {
                return next.depth;
            }
            queue.push_back(next.clone());
        }
    }
    0
}

fn part1(input: &str) -> usize {
    input
        .lines()
        .map(|line| {
            let (target, buttons, _) = parse(line);
            let start = State {
                lights: (0..target.len()).map(|_| false).collect(),
                depth: 0,
            };
            bfs(start, &target, &buttons)
        })
        .sum()
}

fn part1a(input: &str) -> usize {
    input
        .lines()
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

fn solve_ilp(buttons: &[Vec<usize>], joltages: &[usize]) -> usize {
    let mut vars = variables!();
    let press_vars = (0..buttons.len())
        .map(|_| vars.add(variable().min(0).integer()))
        .collect::<Vec<_>>();

    let mut problem = highs(vars.minimise(press_vars.iter().sum::<Expression>()));
    let mut exprs = vec![0.into_expression(); joltages.len()];
    for (i, button) in buttons.iter().enumerate() {
        for &x in button {
            exprs[x] += press_vars[i];
        }
    }
    for (e, &j) in exprs.into_iter().zip(joltages) {
        problem.add_constraint(e.eq(j as f64));
    }
    let sol = problem.solve().unwrap();
    press_vars
        .iter()
        .map(|&v| sol.value(v).round() as usize)
        .sum()
}

fn part2(input: &str) -> usize {
    input
        .lines()
        .map(|line| {
            let (_, buttons, joltages) = parse(line);
            solve_ilp(&buttons, &joltages)
        })
        .sum()
}

fn main() {
    // let input = read_to_string("./inputs/example.txt").unwrap();
    let input = read_to_string("./inputs/2025-12-10.txt").unwrap();

    println!("part1: {:?}", part1(&input));
    println!("part1a: {:?}", part1a(&input));
    println!("part2: {:?}", part2(&input));
}
