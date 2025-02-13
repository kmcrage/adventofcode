use hashbrown::HashMap;
use itertools::Itertools;
use std::fs::read_to_string;

#[derive(Copy, Clone, Debug, Default)]
struct Robot {
    pos: [isize; 2],
    vel: [isize; 2],
}

impl Robot {
    fn from(input: &str) -> Vec<Robot> {
        input
            .lines()
            .map(|line| {
                if let Some((pline, vline)) = line.split_once(" ") {
                    return Robot {
                        pos: Robot::_parse_vector(pline),
                        vel: Robot::_parse_vector(vline),
                    };
                }
                Robot::default()
            })
            .collect()
    }

    fn _parse_vector(input: &str) -> [isize; 2] {
        input
            .split("=")
            .nth(1)
            .unwrap()
            .split(",")
            .map(|i| i.parse::<isize>().unwrap())
            .collect::<Vec<isize>>()
            .try_into()
            .unwrap()
    }

    fn step(&self, size: (isize, isize)) -> Robot {
        Robot {
            pos: [
                (self.pos[0] + self.vel[0] + size.0) % size.0,
                (self.pos[1] + self.vel[1] + size.1) % size.1,
            ],
            vel: [self.vel[0], self.vel[1]],
        }
    }
}

fn safety_factor(robots: &[Robot], size: (isize, isize)) -> usize {
    let mut quads: [usize; 4] = Default::default();
    for robot in robots {
        let i = robot.pos[0] - (size.0 / 2);
        let j = robot.pos[1] - (size.1 / 2);
        if i == 0 || j == 0 {
            continue;
        }
        let mut idx: usize = 0;
        if i > 0 {
            idx += 1;
        }
        if j > 0 {
            idx += 2;
        }
        quads[idx] += 1;
    }
    quads.iter().product()
}

fn is_tree(robots: &[Robot], coord: usize) -> bool {
    let cnt = (robots.len() as f64).sqrt() as usize;
    let mut rows: HashMap<isize, usize> = Default::default();
    for robot in robots {
        rows.entry(robot.pos[coord])
            .and_modify(|r| {
                *r += 1;
            })
            .or_insert(1);
    }
    rows.values().sorted().last().unwrap() > &cnt
}

/* 
fn print_robots(robots: &[Robot], size: (isize, isize)) {
    let pos: HashSet<[isize; 2]> = HashSet::from_iter(robots.iter().map(|r| r.pos));
    for j in 0..size.1 {
        for i in 0..size.0 {
            if pos.contains(&[i, j]) {
                print!("*");
            } else {
                print!(" ");
            }
        }
        println!(" ");
    }
}
*/

fn part1(robots: &[Robot], size: (isize, isize), steps: usize) -> usize {
    let mut robots: Vec<Robot> = robots.to_vec();
    for _ in 0..steps {
        robots = robots.iter().map(|&r| r.step(size)).collect();
    }
    safety_factor(&robots, size)
}

fn part2(robots: &[Robot], size: (isize, isize)) -> usize {
    let mut robots: Vec<Robot> = robots.to_vec();
    let mut steps: usize = 0;
    let mut row: Option<usize> = None;
    let mut col: Option<usize> = None;
    let mut row_p: Option<usize> = None;
    let mut col_p: Option<usize> = None;
    loop {
        robots = robots.iter().map(|&r| r.step(size)).collect();
        steps += 1;
        if is_tree(&robots, 0) {
            if row.is_none() {
                row = Some(steps);
            } else {
                row_p = Some(steps - row.unwrap());
            }
        }
        if is_tree(&robots, 1) {
            if col.is_none() {
                col = Some(steps);
            } else {
                col_p = Some(steps - col.unwrap());
            }
        }
        if row_p.is_some() && col_p.is_some() {
            let mut crt = row.unwrap();
            while crt % col_p.unwrap() != col.unwrap() {
                crt += row_p.unwrap();
            } 
            break crt;
        }
    }
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-14.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let robots = Robot::from(input.as_str());
    // println!("part1: {:?}", part1(&robots, (11, 7), 100));
    println!("part1: {:?}", part1(&robots, (101, 103), 100));
    println!("part2: {:?}", part2(&robots, (101, 103)));
}
