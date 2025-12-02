use std::fs::read_to_string;

fn parse_range(range: &str) -> (i64, i64) {
    let (from, to) = range.split_once('-').unwrap();

    (from.parse::<i64>().unwrap(), to.parse::<i64>().unwrap())
}

fn part1(range: &str) -> i64 {
    let (from, to) = parse_range(range);

    let mut cnt = 0;
    for i in from..=to {
        let len = i.ilog10() + 1;
        let m = 10_i64.pow(len / 2);
        if i / m == i % m {
            cnt += i;
        }
    }
    cnt
}

fn part2(range: &str) -> i64 {
    let (from, to) = parse_range(range);

    let mut cnt = 0;
    for i in from..=to {
        let istr = i.to_string();
        let len = istr.len();
        for m in 1..=(len / 2) {
            if !len.is_multiple_of(m) {
                continue;
            }
            if (m..len).step_by(m).all(|k| istr[0..m] == istr[k..k + m]) {
                cnt += i;
                break;
            }
        }
    }
    cnt
}

fn main() {
    let file = "./inputs/2025-12-02.txt";
    // let file = "./inputs/example.txt";

    let input = read_to_string(file).unwrap();

    let part1: i64 = input.split(',').map(part1).sum();
    println!("part1: {part1}");

    let part2: i64 = input.split(',').map(part2).sum();
    println!("part2: {part2}");
}
//1227775554
//4174379265
