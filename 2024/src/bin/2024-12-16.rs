use binary_heap_plus::BinaryHeap;
use hashbrown::{HashMap, HashSet};
use std::{cmp::Ordering, fs::read_to_string};

struct Maze {
    map: Vec<char>,
    width: usize,
    start: usize,
    end: usize,
    directions: [isize; 4],
}

#[derive(Clone, Debug)]
struct State {
    priority: usize,
    cost: usize,
    position: usize,
    dir: usize,
    path: Vec<usize>,
}

impl State {
    fn cmp_state(a: &State, b: &State) -> Ordering {
        b.priority.cmp(&a.priority)
    }
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

        let step = ((state.position as isize) + self.directions[state.dir]) as usize;
        if Some(&'#') != self.map.get(step) {
            let mut path = state.path.clone();
            path.push(step);
            nhbrs.push(State {
                priority: state.cost + 1 + self.dist_to_end(step),
                cost: state.cost + 1,
                position: step,
                dir: state.dir,
                path,
            });
        }
        nhbrs.push(State {
            priority: state.priority + 1000,
            cost: state.cost + 1000,
            position: state.position,
            dir: (state.dir + 1) % 4,
            path: state.path.clone(),
        });
        nhbrs.push(State {
            priority: state.priority + 1000,
            cost: state.cost + 1000,
            position: state.position,
            dir: (state.dir + 3) % 4,
            path: state.path.clone(),
        });

        nhbrs
    }

    fn shortest_path_len(&self) -> (usize, usize) {
        let results = self.shortest_path();
        let cost = results[0].cost;
        let mut visited: HashSet<usize> = Default::default();
        for result in results {
            visited.extend(&result.path);
        }
        (cost, visited.len())
    }

    fn dist_to_end(&self, pos: usize) -> usize {
        let (ex, ey) = (self.end/self.width, self.end % self.width);
        let (px, py) = (pos/self.width, pos % self.width);
        ex.abs_diff(px) + ey.abs_diff(py)
    }

    fn shortest_path(&self) -> Vec<State> {
        let mut priorities: HashMap<(usize, usize), usize> = Default::default();
        let mut heap = BinaryHeap::new_by(State::cmp_state);
        let mut results: Vec<State> = Default::default();

        heap.push(State {
            priority: self.dist_to_end(self.start),
            cost: 0,
            position: self.start,
            dir: 0,
            path: Vec::from([self.start]),
        });

        // Examine the frontier with lower cost nodes first (min-heap)
        while let Some(state) = heap.pop() {
            // don't go past the cost of finishing
            if !results.is_empty() && results[0].priority < state.priority{
                return results;
            }

            // Important as we may have already found a better way
            if let Some(&priority) = priorities.get(&(state.position, state.dir)) {
                if state.priority > priority {
                    continue;
                }
            }
            // Relaxation, we have now found a better way
            priorities.insert((state.position, state.dir), state.priority);

            // Alternatively we could have continued to find all shortest paths
            if state.position == self.end {
                results.push(state);
                continue;
            }

            // For each node we can reach, see if we can find a way with
            // a lower cost going through this node
            for next in self.nhbrs(&state) {
                // If so, add it to the frontier and continue
                let priority = priorities.get(&(next.position, next.dir));
                if priority.is_none() || priority.unwrap() >= &next.priority {
                    heap.push(next.clone());
                }
            }
        }
        results
    }
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-16.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let maze = Maze::from(input.as_str());
    println!("parts: {:?}", maze.shortest_path_len());
}
//parts: (94436, 481)