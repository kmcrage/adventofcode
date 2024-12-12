use hashbrown::HashSet;
use std::collections::VecDeque;
use std::fs::read_to_string;

struct Plan {
    map: Vec<char>,
    // width: usize,
    directions: [isize; 4],
}

impl Plan {
    fn from(input: &str) -> Plan {
        let width = input.chars().position(|c| c == '\n').unwrap() as isize + 1;
        let directions = [1, width, -1, -width];
        Plan {
            map: input.chars().collect(),
            // width,
            directions,
        }
    }

    fn sides(&self, walls: &HashSet<(usize, usize)>) -> usize {
        walls
            .iter()
            .filter(|(p, d)| {
                let nhbr = *p as isize + self.directions[(d + 1) % 4];
                !walls.contains(&(nhbr as usize, *d))
            })
            .count()
    }

    fn analyse_area(&self, seed: usize, chr: &char) -> (HashSet<usize>, (usize, usize)) {
        let mut walls: HashSet<(usize, usize)> = Default::default(); // (plot, dir)
        let mut visited: HashSet<usize> = Default::default();
        let mut plots: VecDeque<usize> = VecDeque::from([seed]);

        while let Some(plot) = plots.pop_front() {
            if visited.contains(&plot) {
                continue;
            }
            visited.insert(plot);

            for dir in self.directions.iter().enumerate() {
                let wall = (plot, dir.0);
                let next_plot = plot as isize + dir.1;
                if next_plot < 0 {
                    walls.insert(wall);
                    continue;
                }
                let next_plot = next_plot as usize;
                if let Some(next_chr) = self.map.get(next_plot) {
                    if next_chr == chr && !visited.contains(&next_plot) {
                        plots.push_back(next_plot);
                        continue;
                    }
                }
                walls.insert(wall);
            }
        }
        let price1 = visited.len() * walls.len();
        let price2 = visited.len() * self.sides(&walls);
        (visited, (price1, price2))
    }

    fn fence_price(&self) -> (usize, usize) {
        let mut prices: (usize, usize) = (0, 0);
        let mut visited: HashSet<usize> = Default::default();

        for (seed, chr) in self.map.iter().enumerate() {
            if chr == &'\n' || visited.contains(&seed) {
                continue;
            }
            let (vis, price) = self.analyse_area(seed, chr);
            visited.extend(&vis);
            prices.0 += price.0;
            prices.1 += price.1;
        }
        prices
    }
}

fn main() {
    let file = "./inputs/test.txt";
    // let file = "./inputs/2024-12-13.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let plan = Plan::from(input.as_str());
    println!("parts: {:?}", plan.fence_price());
}
