use itertools::Itertools;
use memoize::memoize;
use std::cmp::min;
use std::fs::read_to_string;

fn numpad(key: char) -> (i32, i32) {
    match key {
        '7' => (0, 0),
        '8' => (0, 1),
        '9' => (0, 2),
        '4' => (1, 0),
        '5' => (1, 1),
        '6' => (1, 2),
        '1' => (2, 0),
        '2' => (2, 1),
        '3' => (2, 2),
        '0' => (3, 1),
        'A' => (3, 2),
        _ => panic!(),
    }
}
fn dirpad(key: char) -> (i32, i32) {
    match key {
        '^' => (0, 1),
        'A' => (0, 2),
        '<' => (1, 0),
        'v' => (1, 1),
        '>' => (1, 2),
        _ => panic!(),
    }
}

// move in the direction, then press 'A'
#[memoize]
fn dirpad_presses(dy: i32, dx: i32, v_first: bool, level: usize) -> usize {
    let mut code = String::new();
    let lr = if dx > 0 { ">" } else { "<" };
    code.push_str(&lr.repeat(dx.unsigned_abs() as usize));
    let ud = if dx > 0 { "v" } else { "^" };
    code.push_str(&ud.repeat(dy.unsigned_abs() as usize));
    if v_first {
        code = code.chars().rev().collect();
    }
    code.push('A');
    // println!("dirpad: {} {}", level, code);

    if level == 0 {
        code.len()
    } else {
        code.insert(0, 'A');
        code.chars()
            .tuple_windows::<(_, _)>()
            .map(|(f, t)| dirpad_press(f, t, level))
            .sum()
    }
}

fn dirpad_press(from_chr: char, to_chr: char, level: usize) -> usize {
    let from_coord = dirpad(from_chr);
    let to_coord = dirpad(to_chr);
    let dy = to_coord.0 - from_coord.0;
    let dx = to_coord.1 - from_coord.1;
    if dy == 0 || dx == 0 || to_coord.1 == 0 {
        dirpad_presses(dy, dx, true, level - 1)
    } else if from_coord.1 == 0 {
        dirpad_presses(dy, dx, false, level - 1)
    } else {
        min(
            dirpad_presses(dy, dx, true, level - 1),
            dirpad_presses(dy, dx, false, level - 1),
        )
    }
}

// start at 'A', move to each key and press
fn numpad_presses(input: &str, level: usize) -> usize {
    // println!("numpad: {}", input);
    let mut code = String::from("A");
    code.push_str(input);

    code.chars()
        .filter(char::is_ascii_digit)
        .collect::<String>()
        .parse::<usize>()
        .unwrap()
        * code
            .chars()
            .tuple_windows::<(_, _)>()
            .map(|(f, t)| numpad_press(f, t, level))
            .sum::<usize>()
}

fn numpad_press(from_chr: char, to_chr: char, level: usize) -> usize {
    let from_coord = numpad(from_chr);
    let to_coord = numpad(to_chr);
    let dy = to_coord.0 - from_coord.0;
    let dx = to_coord.1 - from_coord.1;
    if dy == 0 || dx == 0 || (from_coord.0 == 3 && to_coord.1 == 0) {
        dirpad_presses(dy, dx, true, level)
    } else if from_coord.1 == 0 && to_coord.0 == 3 {
        dirpad_presses(dy, dx, false, level)
    } else {
        min(
            dirpad_presses(dy, dx, true, level),
            dirpad_presses(dy, dx, false, level),
        )
    }
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-21.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));

    println!(
        "part1: {}",
        input.lines().map(|l| numpad_presses(l, 2)).sum::<usize>()
    );
    println!(
        "part2: {}",
        input.lines().map(|l| numpad_presses(l, 25)).sum::<usize>()
    );
}
