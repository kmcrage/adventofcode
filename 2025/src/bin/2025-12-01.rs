use std::fs::read_to_string;

fn counter(acc: (i64, i64, i64), mv: &str) -> (i64, i64, i64) {
    let sz = mv[1..].parse::<i64>().unwrap();
    let (mut pt1, mut pt2, mut pos) = acc;

    // full rotations
    pt2 += sz / 100;
    let sz = sz % 100;

    match mv.chars().next().unwrap() {
        'R' => {
            pos += sz;
            if pos > 99 {
                pt2 += 1;
            }
        }
        'L' => {
            if pos != 0 && sz >= pos {
                pt2 += 1;
            }
            pos -= sz;
        }
        _ => panic!("invalid direction"),
    }

    pos = (100 + pos) % 100;

    // stop at zero
    if pos == 0 {
        pt1 += 1;
    }
    (pt1, pt2, pos)
}

fn main() {
    let file = "./inputs/2025-12-01.txt";

    let input = read_to_string(file).unwrap();

    let parts: (i64, i64, i64) = input.lines().fold((0, 0, 50), counter);
    println!("part1: {0}", parts.0);
    println!("part2: {0}", parts.1);
}
