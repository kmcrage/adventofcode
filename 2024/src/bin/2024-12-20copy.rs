use binary_heap_plus::BinaryHeap;
use hashbrown::{HashMap, HashSet};
use std::cmp::min;
use std::fs::read_to_string;

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
struct Solve {
    length: usize,
    map: HashMap<usize, usize>,
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

    fn shortcuts(&self, solve: &Solve, state: &State, speedup: usize, cheat: usize) -> usize {
        let mut cut = 0;
        let (px, py) = (state.position % self.width, state.position / self.width);

        for i in px.saturating_sub(cheat)..=min(px + cheat, self.width - 1) {
            for j in py.saturating_sub(cheat)..=min(py + cheat, self.width - 1) {
                let jump = i.abs_diff(px) + j.abs_diff(py);
                if jump > cheat {
                    continue;
                }
                let next = i + j * self.width;
                if let Some(&nxt_cost) = solve.map.get(&next) {
                    if nxt_cost + speedup + jump + state.cost <= solve.length {
                        cut += 1;
                    }
                }
            }
        }
        cut
    }

    fn shortest_fwd_path(&self) -> Solve {
        let mut costs: HashMap<usize, usize> = Default::default();
        let mut heap = BinaryHeap::new_by(|a: &State, b: &State| b.cost.cmp(&a.cost));

        heap.push(State {
            cost: 0,
            position: self.start,
            cheats: Default::default(),
        });

        // Examine the frontier with lower cost nodes first (min-heap)
        while let Some(state) = heap.pop() {
            // Alternatively we could have continued to find all shortest paths
            if state.position == self.end {
                costs.insert(state.position, state.cost);
                return Solve {
                    length: state.cost,
                    map: costs,
                };
            }

            // Important as we may have already found a better way
            if let Some(&cost) = costs.get(&state.position) {
                if state.cost >= cost {
                    continue;
                }
            }
            // Relaxation, we have now found a better way
            costs.insert(state.position, state.cost);

            // For each node we can reach, see if we can find a way with
            // a lower cost going through this node
            for next in self.nhbrs(&state) {
                // If so, add it to the frontier and continue
                match costs.get(&next.position) {
                    None => {
                        heap.push(next.clone());
                    }
                    Some(&cost) => {
                        if cost > state.cost {
                            heap.push(next.clone());
                        }
                    }
                }
            }
        }
        Solve {
            length: 0,
            map: Default::default(),
        }
    }

    fn shortest_bkwd_path(&self, solve: &Solve, speedup: usize, cheat: usize) -> usize {
        let mut heap = BinaryHeap::new_by(|a: &State, b: &State| b.cost.cmp(&a.cost));
        let mut visited: HashSet<usize> = Default::default();
        let mut rt = 0;

        heap.push(State {
            cost: 0,
            position: self.end,
            cheats: Default::default(),
        });

        // Examine the frontier with lower cost nodes first (min-heap)
        while let Some(state) = heap.pop() {
            if state.position == self.start || state.cost + speedup > solve.length {
                return rt;
            }

            // using a heap ensures the first visit is the best
            if visited.contains(&state.position) {
                continue;
            }
            visited.insert(state.position);

            // find all the shortcuts from this point
            rt += self.shortcuts(solve, &state, speedup, cheat);

            for next in self.nhbrs(&state) {
                if !visited.contains(&next.position) {
                    heap.push(next.clone());
                }
            }
        }
        rt
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
