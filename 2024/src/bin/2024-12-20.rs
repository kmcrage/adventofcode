use hashbrown::{HashMap, HashSet};
use std::fs::read_to_string;
use std::collections::VecDeque;

struct Maze {
    map: Vec<char>,
    width: usize,
    start: usize,
    end: usize,
    directions: [isize; 4],
}

#[derive(Clone, Default, Debug)]
struct State {
    cost: usize,
    position: usize,
    cheats: Vec<usize>,
}

#[derive(Clone, Default, Debug)]
struct Solution {
    length: usize,
    to_start: HashMap<usize, usize>,
}

impl Maze {
    fn from(input: &str) -> Maze {
        let map: Vec<char> = input.chars().collect();
        let start = map.iter().position(|c| c == &'S').unwrap();
        let end = map.iter().position(|c| c == &'E').unwrap();
        let width = map.iter().position(|c| c == &'\n').unwrap() + 1;
        let directions = [1, width as isize, -1, -(width as isize)];
        Maze {
            map,
            width,
            start,
            end,
            directions,
        }
    }
    fn nhbrs(&self, state: &State) -> Vec<State> {
        let mut nhbrs: Vec<State> = Default::default();

        for dir in self.directions {
            let next = (state.position as isize + dir) as usize;
            if Some(&'#') != self.map.get(next) {
                nhbrs.push(State {
                    cost: state.cost + 1,
                    position: next,
                    cheats: state.cheats.clone(),
                });
            }
        }

        nhbrs
    }

    fn shortcuts(&self, solution: &Solution, state: &State, speedup: usize, cheat: usize) -> usize {
        let mut cut = 0;
        let (px, py) = (state.position % self.width, state.position / self.width);

        for dx in -(cheat as isize)..=cheat as isize {
            let x= px as isize + dx;
            if x <0 || x >= self.width as isize {
                continue;
            }
            let x= x as usize;
            for dy in (-(cheat as isize)+dx.abs())..=(cheat as isize - dx.abs()) {
                let y= py as isize + dy;
                if y <0 || y >= self.width as isize {
                    continue;
                }
                let y= y as usize;

                let next = x + y * self.width;
                if let Some(&to_start) = solution.to_start.get(&next) {
                    if to_start + speedup + dx.unsigned_abs() + dy.unsigned_abs() + state.cost <= solution.length {
                        cut += 1;
                    }
                }
            }
        }
        cut
    }

    fn shortest_fwd_path(&self) -> Solution {
        let mut visited: HashMap<usize, usize> = Default::default();
        let mut queue: VecDeque<State> = Default::default();
        queue.push_back(State {
            cost: 0,
            position: self.start,
            cheats: Default::default(),
        });

        // Examine the frontier with lower cost nodes first (min-heap)
        while let Some(state) = queue.pop_front() {
            // Alternatively we could have continued to find all shortest paths
            if state.position == self.end {
                visited.insert(state.position, state.cost);
                return Solution {
                    length: state.cost,
                    to_start: visited,
                };
            }

            // Important as we may have already found a better way
            if let Some(&cost) = visited.get(&state.position) {
                if state.cost >= cost {
                    continue;
                }
            }
            // Relaxation, we have now found a better way
            visited.insert(state.position, state.cost);

            // For each node we can reach, see if we can find a way with
            // a lower cost going through this node
            for next in self.nhbrs(&state) {
                // If so, add it to the frontier and continue
                match visited.get(&next.position) {
                    None => {
                        queue.push_back(next);
                    }
                    Some(&cost) => {
                        if cost > state.cost {
                            queue.push_back(next);
                        }
                    }
                }
            }
        }
        Solution {
            length: 0,
            to_start: Default::default(),
        }
    }

    fn shortest_bkwd_path(&self, solve: &Solution, speedup: usize, cheat: usize) -> usize {
        let mut queue: VecDeque<State> = Default::default();
        let mut visited: HashSet<usize> = Default::default();
        let mut shortcuts = 0;

        queue.push_back(State {
            cost: 0,
            position: self.end,
            cheats: Default::default(),
        });

        // Examine the frontier with lower cost nodes first (min-heap)
        while let Some(state) = queue.pop_front() {
            if state.position == self.start || state.cost + speedup > solve.length {
                return shortcuts;
            }

            // using a heap ensures the first visit is the best
            if visited.contains(&state.position) {
                continue;
            }
            visited.insert(state.position);

            // find all the shortcuts from this point
            shortcuts += self.shortcuts(solve, &state, speedup, cheat);

            for next in self.nhbrs(&state) {
                if !visited.contains(&next.position) {
                    queue.push_back(next);
                }
            }
        }
        shortcuts
    }
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-20.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let maze = Maze::from(input.as_str());
    let honest = maze.shortest_fwd_path();
    println!("part0: {:?}", honest.length);
    // println!("part1: {:?}", maze.shortest_bkwd_path(&honest, 20, 2));
    // println!("part2: {:?}", maze.shortest_bkwd_path(&honest, 74, 20));
    println!("part1: {:?}", maze.shortest_bkwd_path(&honest, 100, 2));
    println!("part2: {:?}", maze.shortest_bkwd_path(&honest, 100, 20));
}
