//use hashbrown::HashSet;
use itertools::Itertools;
//use std::collections::VecDeque;
use std::fs::read_to_string;

#[derive(Debug, Default)]
struct Claw {
    a: (isize, isize),
    b: (isize, isize),
    prize: (isize, isize),
}

impl Claw {
    fn from(inputs: &str) -> Vec<Claw> {
        let mut claws: Vec<Claw> = Default::default();

        for chunk in inputs.split("\n\n") {
            let lines = chunk.lines().collect_vec();

            let mut claw: Claw = Default::default();
            claw.a.0 = Claw::_parse_button(lines[0], 0);
            claw.a.1 = Claw::_parse_button(lines[0], 1);
            claw.b.0 = Claw::_parse_button(lines[1], 0);
            claw.b.1 = Claw::_parse_button(lines[1], 1);
            claw.prize.0 = Claw::_parse_prize(lines[2], 0);
            claw.prize.1 = Claw::_parse_prize(lines[2], 1);

            claws.push(claw);
        }
        claws
    }

    fn _parse_prize(input: &str, pos: usize) -> isize {
        Claw::_parse_generic(input, "=", pos)
    }

    fn _parse_button(input: &str, pos: usize) -> isize {
        Claw::_parse_generic(input, "+", pos)
    }

    fn _parse_generic(input: &str, spt: &str, pos: usize) -> isize {
        input
            .split(spt)
            .nth(pos + 1)
            .unwrap()
            .split(",")
            .next()
            .unwrap()
            .parse::<isize>()
            .unwrap()
    }

    fn cost(&self, small: bool) -> isize {
        let mut prize = self.prize;
        if !small {
            prize = (prize.0 + 10000000000000, prize.1 + 10000000000000);
        }

        // non-invertible...
        let det = self.a.0 * self.b.1 - self.a.1 * self.b.0;
        if det == 0 {
            return 0;
        }
        let press = (
            self.b.1 * prize.0 - self.b.0 * prize.1,
            self.a.0 * prize.1 - self.a.1 * prize.0,
        );

        // ...fractional presses...
        if press.0 % det != 0 || press.1 % det != 0 {
            return 0;
        }
        let press = (press.0 / det, press.1 / det);

        // ...too many presses...
        if small && (!(0..=100).contains(&(press.0)) || !(0..=100).contains(&(press.1))) {
            return 0;
        }

        3 * press.0 + press.1
    }
}

fn cost(claws: &[Claw], small: bool) -> isize {
    claws.iter().map(|c| c.cost(small)).sum()
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-13.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let claws = Claw::from(input.as_str());
    println!("part1: {:?}", cost(&claws, true));
    println!("part2: {:?}", cost(&claws, false));
}
