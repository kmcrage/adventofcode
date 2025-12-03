use binary_heap_plus::BinaryHeap;
use hashbrown::HashMap;
use std::cmp::Ordering;
use std::fs::read_to_string;

struct Memory {
    bytes: HashMap<usize, usize>,
    results: Vec<String>,
    width: usize,
    start: usize,
    end: usize,
    directions: [isize; 4],
}

#[derive(Clone, Debug)]
struct State {
    position: usize,
    priority: usize,
    path_len: usize,
}

impl State {
    fn cmp_state(a: &State, b: &State) -> Ordering {
        b.priority.cmp(&a.priority)
    }
}

impl Memory {
    fn from(input: &str, max: usize) -> Self {
        let width = max + 3; // allow boundaries
        let directions = [1, width as isize, -1, -(width as isize)];
        let results = input.lines().map(String::from).collect();
        let bytes = input
            .lines()
            .enumerate()
            .map(|(n, line)| {
                let (a, b) = line.split_once(",").unwrap();
                (
                    (1 + a.parse::<usize>().unwrap()) * width + b.parse::<usize>().unwrap() + 1,
                    n,
                )
            })
            .collect();
        Memory {
            bytes,
            results,
            width,
            start: 1 + width,
            end: width - 2 + (width - 2) * width, // offset by one
            directions,
        }
    }

    fn nhbrs(&self, state: &State, bytes: usize) -> Vec<State> {
        let mut nhbrs: Vec<State> = Default::default();

        for dir in self.directions {
            let nxt_pos = (state.position as isize + dir) as usize;

            if (nxt_pos / self.width == 0)
                || (nxt_pos / self.width == self.width - 1)
                || nxt_pos.is_multiple_of(self.width)
                || (nxt_pos % self.width == self.width - 1)
            {
                continue;
            }

            if let Some(&b) = self.bytes.get(&nxt_pos) {
                if b < bytes {
                    continue;
                }
            }

            nhbrs.push(State {
                position: nxt_pos,
                priority: state.path_len + 1 + self.dist_to_end(nxt_pos),
                path_len: state.path_len + 1,
            });
        }
        nhbrs
    }

    fn dist_to_end(&self, pos: usize) -> usize {
        let (ex, ey) = (self.end / self.width, self.end % self.width);
        let (px, py) = (pos / self.width, pos % self.width);
        ex.abs_diff(px) + ey.abs_diff(py)
    }

    fn shortest_path(&self, bytes: usize) -> Option<usize> {
        let mut priorities: HashMap<usize, usize> = Default::default();
        let mut heap = BinaryHeap::new_by(State::cmp_state);

        heap.push(State {
            priority: self.dist_to_end(self.start),
            position: self.start,
            path_len: 0,
        });

        // Examine the frontier with lower cost nodes first (min-heap)
        while let Some(state) = heap.pop() {
            // Important as we may have already found a better way
            if let Some(&priority) = priorities.get(&state.position) {
                if state.priority >= priority {
                    continue;
                }
            }
            // Relaxation, we have now found a better way
            priorities.insert(state.position, state.priority);

            // Alternatively we could have continued to find all shortest paths
            if state.position == self.end {
                return Some(state.path_len);
            }

            // For each node we can reach, see if we can find a way with
            // a lower cost going through this node
            for nhbr in self.nhbrs(&state, bytes) {
                // If so, add it to the frontier and continue
                let priority = priorities.get(&nhbr.position);
                if priority.is_none() || &nhbr.priority < priority.unwrap() {
                    heap.push(nhbr.clone());
                }
            }
        }
        None
    }

    fn blocker(&self) -> String {
        let mut min: usize = 0;
        let mut max = self.bytes.len().saturating_sub(1);
        while min.abs_diff(max) > 1 {
            let mid = (min + max) / 2;
            if self.shortest_path(mid).is_none() {
                max = mid;
            } else {
                min = mid;
            }
        }
        self.results.get(min).unwrap().to_string()
    }
}

fn main() {
    /*
    let file = "./inputs/test.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let memory = Memory::from(input.as_str(), 6);
    println!("part1: {:?}", memory.shortest_path(12).unwrap());
    println!("part2: {:?}", memory.blocker());
    */

    let file = "./inputs/2024-12-18.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let memory = Memory::from(input.as_str(), 70);
    println!("part1: {:?}", memory.shortest_path(1024).unwrap());
    println!("part2: {:?}", memory.blocker());
}
