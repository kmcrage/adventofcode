use hashbrown::HashMap;
use std::collections::VecDeque;
use std::fs::read_to_string;

struct Guide {
    map: Vec<char>,
    trailheads: Vec<usize>,
    width: usize,
}

impl Guide {
    fn from(input: &str) -> Guide {
        Guide {
            map: input.chars().collect(),
            trailheads: input
                .chars()
                .enumerate()
                .filter(|(_, c)| c == &'0')
                .map(|(p, _)| p)
                .collect(),
            width: input.chars().position(|c| c == '\n').unwrap() + 1,
        }
    }

    fn peaks(&self, trailhead: usize) -> (usize, usize) {
        let mut peaks: HashMap<usize, usize> = Default::default();
        let mut posns: VecDeque<(usize, usize)> = Default::default(); // (posn, height)

        posns.push_back((trailhead, 0));
        while !posns.is_empty() {
            let (posn, height) = posns.pop_front().unwrap();

            // hit a 9
            if let Some(chr) = self.map.get(posn) {
                if chr == &'9' {
                    peaks.entry(posn).and_modify(|c| *c += 1).or_insert(1);
                    continue;
                }
            }

            // walk next step
            let dirs: Vec<isize> = vec![-1, 1, self.width as isize, -(self.width as isize)];
            for dir in dirs {
                let next = posn as isize + dir;
                if next < 0 {
                    continue;
                }

                let next = next as usize;
                if let Some(chr) = self.map.get(next) {
                    if chr.is_ascii_digit()
                        && height + 1 == chr.to_string().parse::<usize>().unwrap()
                    {
                        posns.push_back((next, height + 1));
                    }
                }
            }
        }

        (peaks.len(), peaks.values().sum())
    }

    fn sos(&self) -> (usize, usize) {
        self.trailheads
            .iter()
            .map(|&th| self.peaks(th))
            .fold((0, 0), |acc, x| (acc.0 + x.0, acc.1 + x.1))
    }
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-10.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let guide = Guide::from(&input);
    println!("parts: {:?}", guide.sos());
}
