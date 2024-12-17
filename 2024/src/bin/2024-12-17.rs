use num::pow;
use std::fs::read_to_string;

fn from(input: &str) -> ([usize; 3], Vec<usize>) {
    let (input_state, input_program) = input.split_once("\n\n").unwrap();
    let program = input_program
        .split_whitespace()
        .nth(1)
        .unwrap()
        .split(",")
        .map(|n| n.parse::<usize>().unwrap())
        .collect();
    let mut state = [0, 0, 0];
    for (i, line) in input_state.lines().enumerate() {
        state[i] = line.split_once(": ").unwrap().1.parse::<usize>().unwrap();
    }

    (state, program)
}

fn combo(state: &[usize; 3], operand: usize) -> usize {
    if (0..=3).contains(&operand) {
        return operand % 8;
    }
    if (4..=6).contains(&operand) {
        return state[(operand as isize - 4) as usize] % 8;
    }
    0
}

fn run_program(origin: &[usize; 3], program: &[usize]) -> Vec<usize> {
    let mut state = *origin;
    let mut ptr: usize = 0;
    let mut out: Vec<usize> = Default::default();

    while ptr < program.len() {
        let opcode = program[ptr];
        let operand = program[ptr + 1];
        let mut nxt_ptr = None;

        if opcode == 0 {
            state[0] /= pow(2, combo(&state, operand));
        } else if opcode == 1 {
            state[1] ^= operand;
        } else if opcode == 2 {
            state[1] = combo(&state, operand) % 8;
        } else if opcode == 3 {
            if state[0] != 0 && operand != ptr {
                nxt_ptr = Some(operand);
            }
        } else if opcode == 4 {
            state[1] ^= state[2];
        } else if opcode == 5 {
            out.push(combo(&state, operand));
        } else if opcode == 6 {
            state[1] = state[0] / pow(2, combo(&state, operand));
        } else if opcode == 7 {
            state[2] = state[0] / pow(2, combo(&state, operand));
        }
        if nxt_ptr.is_some() {
            ptr = nxt_ptr.unwrap();
        } else {
            ptr += 2;
        }
    }
    out
}

fn part1(origin: &[usize; 3], program: &[usize]) -> String {
    let out = run_program(origin, program);
    out.iter()
        .map(|&n| n.to_string())
        .collect::<Vec<String>>()
        .join(",")
}

fn part2(program: &[usize]) -> usize {
    let div = 16;
    let mut max = pow(8_usize, program.len()) as usize;
    let mut min = max / div;
    let mut idx = program.len() - 1;

    loop {
        if idx == 1 {
            println!("check final interval, width {}", min.abs_diff(max));
            for i in min..=max {
                if run_program(&[i, 0, 0], program) == program {
                    return i;
                }
            }
            panic!("final interval had no solution!");
        }

        let tests: Vec<(usize, Vec<usize>)> = (0..=div)
            .map(|i| {
                let t = min + (i * min.abs_diff(max)) / div;
                (t, run_program(&[t, 0, 0], program))
            })
            .collect();

        let hits: Vec<usize> = tests
            .iter()
            .enumerate()
            .filter(|(_, (_, out))| out.len() == program.len() && out[idx..] == program[idx..])
            .map(|(i, _)| i)
            .collect();
        if hits.is_empty() {
            continue;
        }

        let min_p = hits[0].saturating_sub(1);
        let mut max_p = hits[0] + 1;
        if hits.len() > 1 {
            while tests[max_p].1[idx..] == program[idx..] {
                max_p += 1;
            }
            idx = idx.saturating_sub(1);
        }
        min = tests[min_p].0;
        max = tests[max_p].0;
    }
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-17.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    let (state, program) = from(input.as_str());
    println!("part1: {:?}", part1(&state, &program));
    println!("part2: {:?}", part2(&program));
}
