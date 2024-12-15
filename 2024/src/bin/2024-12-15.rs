use hashbrown::{HashMap, HashSet};
use std::collections::VecDeque;
use std::fs::read_to_string;

#[derive(Default, Clone)]
struct Room {
    map: Vec<char>,
    moves: Vec<char>,
    width: usize,
    dirs: HashMap<char, isize>,
}

impl Room {
    fn from(input: &str, wide: bool) -> Room {
        let (map, moves) = input.split_once("\n\n").unwrap();
        let mut map: Vec<char> = map.chars().collect();
        if wide {
            map = map
                .iter()
                .flat_map(|c| {
                    match c {
                        'O' => ['[', ']'],
                        '#' => ['#', '#'],
                        '@' => ['@', '.'],
                        '.' => ['.', '.'],
                        _ => ['#', '\n'], // actually '\n'
                    }
                })
                .collect();
        }

        let width = map.iter().position(|c| c == &'\n').unwrap() + 1;
        Room {
            map,
            moves: moves.chars().filter(|c| c != &'\n').collect(),
            width,
            dirs: HashMap::from_iter([
                ('>', 1),
                ('<', -1),
                ('^', -(width as isize)),
                ('v', width as isize),
            ]),
        }
    }

    fn move_gps(&self) -> usize {
        let mut moves: VecDeque<char> = self.moves.iter().cloned().collect();
        let mut map = self.map.clone();
        let mut pos = map.iter().position(|c| c == &'@').unwrap();

        while let Some(mv) = moves.pop_front() {
            if let Some(dir) = self.dirs.get(&mv) {
                let nxt = (pos as isize + dir) as usize;
                let nxt_chr = map.get(nxt);
                if nxt_chr.is_none() {
                    continue;
                }
                match nxt_chr.unwrap() {
                    '.' => {
                        map[pos] = '.';
                        map[nxt] = '@';
                        pos = nxt;
                    }
                    'O' | '[' | ']' => {
                        let poss: HashSet<usize> = HashSet::from([nxt]);
                        if let Some(nxt_map) = do_move(&map, &poss, dir) {
                            map = nxt_map;
                            map[pos] = '.';
                            pos = nxt;
                        }
                    }
                    _ => {}
                }
            }
        }
        self.gpscoords(&map)
    }

    fn gpscoords(&self, map: &[char]) -> usize {
        let mut gps = 0;
        for (p, c) in map.iter().enumerate() {
            if c == &'O' || c == &'[' {
                gps += (p % self.width) + 100 * (p / self.width);
            }
        }
        gps
    }
}

fn do_move(map: &[char], poss: &HashSet<usize>, dir: &isize) -> Option<Vec<char>> {
    if poss.is_empty() {
        return Some(map.to_vec());
    }

    let mut nxt_poss: HashSet<usize> = Default::default();
    for p in poss {
        match map.get(*p) {
            Some('#') => {
                return None;
            }
            Some('O') => {
                nxt_poss.insert(((*p as isize) + dir) as usize);
            }
            Some('[') => {
                nxt_poss.insert(((*p as isize) + dir) as usize);
                if dir.abs() != 1 {
                    nxt_poss.insert(((*p as isize) + dir + 1) as usize);
                }
            }
            Some(']') => {
                if dir.abs() != 1 {
                    nxt_poss.insert(((*p as isize) + dir - 1) as usize);
                }
                nxt_poss.insert(((*p as isize) + dir) as usize);
            }
            _ => {}
        }
    }
    match do_move(map, &nxt_poss, dir) {
        Some(nxt_map) => {
            let mut result = nxt_map.clone();
            for &p in poss {
                result[p] = map[((p as isize) - dir) as usize];
                result[((p as isize) - dir) as usize] = '.';
            }
            Some(result)
        }
        None => None,
    }
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-15.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let room = Room::from(input.as_str(), false);
    println!("part1: {:?}", room.move_gps());

    let room_wide = Room::from(input.as_str(), true);
    println!("part2: {:?}", room_wide.move_gps());
}
