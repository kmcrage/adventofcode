use hashbrown::{HashMap, HashSet};
use num::Integer;
use std::fs::read_to_string;

struct State {
    transmitters: HashMap<char, Vec<(isize, isize)>>,
    width: usize,
    height: usize,
}

impl State {
    fn from(input: String) -> State {
        let width = input.chars().position(|c| c == '\n').unwrap();
        let height = input.lines().count();
        let mut transmitters: HashMap<char, Vec<(isize, isize)>> = Default::default();

        for (p, chr) in input.chars().enumerate() {
            if chr != '.' && chr != '\n' {
                // add 1 to width to account for '\n'
                let coord = ((p / (1 + width)) as isize, (p % (1 + width)) as isize);
                match transmitters.get_mut(&chr) {
                    Some(coords) => {
                        coords.push(coord);
                    }
                    None => {
                        transmitters.insert(chr, vec![coord]);
                    }
                }
            }
        }

        State {
            transmitters,
            width,
            height,
        }
    }

    fn antinodes1(&self) -> HashSet<(isize, isize)> {
        let mut antinodes: HashSet<(isize, isize)> = Default::default();

        for nodes in self.transmitters.values() {
            for (i, &n1) in nodes.iter().enumerate() {
                for &n2 in nodes[i + 1..].iter() {
                    for (i, j) in [
                        (2 * n1.0 - n2.0, 2 * n1.1 - n2.1),
                        (2 * n2.0 - n1.0, 2 * n2.1 - n1.1),
                    ] {
                        if 0 <= i && i < self.height as isize && 0 <= j && j < self.width as isize {
                            antinodes.insert((i, j));
                        }
                    }
                }
            }
        }

        antinodes
    }

    fn antinodes2(&self) -> HashSet<(isize, isize)> {
        let mut antinodes: HashSet<(isize, isize)> = Default::default();

        for nodes in self.transmitters.values() {
            for (i, &n1) in nodes.iter().enumerate() {
                for &n2 in nodes[i + 1..].iter() {
                    let mut dir: (isize, isize) = (n1.0 - n2.0, n1.1 - n2.1);
                    if dir.0 == 0 {
                        dir = (0, 1);
                    } else if dir.1 == 0 {
                        dir = (1, 0)
                    } else {
                        let gcd = dir.0.gcd(&dir.1);
                        dir = (dir.0 / gcd, dir.1 / gcd);
                    }

                    for fwd in [1_isize, -1_isize] {
                        let mut pos = n1;
                        while 0 <= pos.0
                            && pos.0 < self.height as isize
                            && 0 <= pos.1
                            && pos.1 < self.width as isize
                        {
                            antinodes.insert(pos);
                            pos.0 += fwd * dir.0;
                            pos.1 += fwd * dir.1;
                        }
                    }
                }
            }
        }

        antinodes
    }
}

fn part1(state: &State) -> usize {
    state.antinodes1().len()
}

fn part2(state: &State) -> usize {
    state.antinodes2().len()
}

fn main() {
    let file = "./inputs/2024-12-08.txt";
    // let file = "./inputs/test.txt";

    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let state = State::from(input);
    println!("part1: {}", part1(&state));
    println!("part2: {}", part2(&state));
}
