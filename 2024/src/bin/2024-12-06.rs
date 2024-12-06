use hashbrown::HashSet;
use rayon::prelude::*;
use std::fs::read_to_string;

#[derive(Clone)]
struct State {
    grid: Vec<char>,
    dir: usize,
    pos: isize,
    width: usize,
}

const DIRECTION: [(isize, isize); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];

impl State {
    fn from(input: String) -> State {
        State {
            grid: input.chars().collect(),
            dir: 0,
            pos: input.chars().position(|c| c == '^').unwrap() as isize,
            width: input.lines().next().unwrap().len() + 1, // extra char for edge of grid
        }
    }

    fn walk(&self) -> Option<HashSet<isize>> {
        let mut pos = self.pos;
        let mut dir = self.dir;
        let mut visited: HashSet<(isize, usize)> = Default::default();

        loop {
            // steps += 1;
            visited.insert((pos, dir));

            let next_pos = pos + (self.width as isize) * DIRECTION[dir].0 + DIRECTION[dir].1;

            if visited.contains(&(next_pos, dir)) {
                break None; //this is a loop
            }

            if let Some(&next_chr) = self.grid.get(next_pos as usize) {
                if next_chr == '#' {
                    dir = (dir + 1) % 4;
                } else if next_chr == '\n' {
                    // off the side of the grid
                    break Some(visited.iter().map(|(p, _)| *p).collect());
                } else {
                    pos = next_pos;
                }
            } else {
                // off the top or bottom of the grid
                break Some(visited.iter().map(|(p, _)| *p).collect());
            }
        }
    }
}

fn part1(origin: &State) -> usize {
    origin.walk().unwrap().len()
}

fn part2(origin: &State) -> usize {
    origin
        .walk()
        .unwrap()
        .par_iter()
        .filter(|pos| {
            // no obstacle in guards start position
            if origin.pos == **pos {
                return false;
            }

            let mut state = origin.clone();
            state.grid[**pos as usize] = '#';

            match state.walk() {
                Some(_v) => false,
                None => true,
            }
        })
        .count()
}

fn main() {
    let file = "./inputs/2024-12-06.txt";
    // let file = "./inputs/test.txt";

    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let state = State::from(input);
    println!("part1: {}", part1(&state));
    println!("part2: {}", part2(&state));
}
