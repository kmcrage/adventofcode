use binary_heap_plus::BinaryHeap;
use hashbrown::{HashMap,HashSet};
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
    fn nhbrs_fwd(&self, state: &State) -> Vec<State> {
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

    fn nhbrs_bkwd(&self, state: &State) -> Vec<State> {
        let mut nhbrs: Vec<State> = Default::default();

        for dir in self.directions {
            let next = state.position as isize + dir;
            if next <0 {continue;}
            let next = next as usize;
            let mut cheats = state.cheats.clone();
            if state.cheats.len() == 1 {
                cheats.push(next);
            }
            if Some(&'#') != self.map.get(next) {
                nhbrs.push(State {
                    cost: state.cost + 1,
                    position: next,
                    cheats,
                });
            } else if state.cheats.is_empty() {
                cheats.push(next);
                for dir in self.directions {
                    let next = next as isize + dir;
                    if next <0 {continue;}
                    let next = next as usize;
                    let mut cheats = cheats.clone(); 
                    cheats.push(next);
                    nhbrs.push(State {
                        cost: state.cost + 2,
                        position: next,
                        cheats,
                    });
                }
            }
        }

        nhbrs
    }

    fn shortest_fwd_path(&self) -> (usize, HashMap<usize, usize>) {
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
                return (state.cost, costs);
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
            for next in self.nhbrs_fwd(&state) {
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
        (0, Default::default())
    }

    fn shortest_bkwd_path(&self, fwd: &(usize, HashMap<usize, usize>)) -> usize {
        let costs = fwd.1.clone();
        let mut heap = BinaryHeap::new_by(|a: &State, b: &State| b.cost.cmp(&a.cost));
        let mut routes :HashSet<(usize,usize)>= Default::default();

        heap.push(State {
            cost: 0,
            position: self.end,
            cheats: Default::default(),
        });

        // Examine the frontier with lower cost nodes first (min-heap)
        while let Some(state) = heap.pop() {
            //println!("{:?}", state);
            // Alternatively we could have continued to find all shortest paths
            if state.position == self.start {
                routes.insert((state.cheats[0],state.cheats[1]));
                continue;
            }
            if state.cost > fwd.0 - 100{
                break;
            }

            // Important as we may have already found a better way
            /*if let Some(&cost) = costs.get(&state.position) {
                if state.cost > cost {
                    continue;
                }
            }*/
            // Relaxation, we have now found a better way
            //costs.insert(state.position, state.priority);

            // For each node we can reach, see if we can find a way with
            // a lower cost going through this node
            for next in self.nhbrs_bkwd(&state) {
                // println!("  next: {:?}", next);
                // If so, add it to the frontier and continue
                if let Some(nxt_cost) = costs.get(&next.position) {
                    if let Some(state_cost) = costs.get(&state.position) {
                        if nxt_cost < state_cost {
                            heap.push(next.clone());
                        }
                    } 
                } 
            }
        }
        routes.len()
    }
}

fn main() {
    //let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-20.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let maze = Maze::from(input.as_str());
    let honest = maze.shortest_fwd_path();
    println!("part1: {:?}", honest.0);
    println!("part2: {:?}", maze.shortest_bkwd_path(&honest ));
}
//parts: (94436, 481)
