use hashbrown::HashSet;
use rayon::prelude::*;
use std::fs::read_to_string;

#[derive(Clone)]
struct State {
    grid: Vec<char>,
    dir: usize,
    pos: isize,
    direction: [isize; 4],
}

impl State {
    fn from(input: String) -> State {
        let width = input.lines().next().unwrap().len() as isize + 1;
        State {
            grid: input.chars().collect(),
            dir: 0,
            pos: input.chars().position(|c| c == '^').unwrap() as isize,
            direction: [-width, 1, width, -1],
        }
    }

    // if brick == None, we want a full path to use to decide where to put bricks
    //
    // If brick == Some, we only need check the brick positions in dir 0 for loops
    //
    fn walk(&self, brick: Option<isize>) -> Option<HashSet<isize>> {
        let mut pos = self.pos;
        let mut dir = self.dir;
        let mut visited: HashSet<(isize, usize)> = Default::default();

        loop {
            let next_pos = pos + self.direction[dir];
            if brick.is_none() {
                visited.insert((pos, dir));
                if visited.contains(&(next_pos, dir)) {
                    break None; //this is a loop
                }
            }

            if let Some(&next_chr) = self.grid.get(next_pos as usize) {
                if next_chr == '#' || (brick.is_some() && next_pos == brick.unwrap()) {
                    dir = (dir + 1) % 4;
                    if brick.is_some() && dir == 0 {
                        if visited.contains(&(next_pos, dir)) {
                            break None; //this is a loop
                        }
                        visited.insert((next_pos, dir));
                    }
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
    origin.walk(None).unwrap().len()
}

fn part2(origin: &State) -> usize {
    origin
        .walk(None)
        .unwrap()
        .par_iter()
        .filter(|pos| {
            // no obstacle in guards start position
            if origin.pos == **pos {
                return false;
            }

            match origin.walk(Some(**pos)) {
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
