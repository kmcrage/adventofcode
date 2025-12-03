use std::fs::read_to_string;

fn joltage(bank: &str, sz: usize) -> i64 {
    bank.chars()
        .fold(vec![0_i64; sz], |mut jolts, chr| {
            let d = chr.to_digit(10).unwrap_or(0) as i64;
            jolts.push(d);
            for i in 0..sz {
                if jolts[i + 1] > jolts[i] {
                    jolts.remove(i);
                    break;
                }
            }
            if jolts.len() > sz {
                jolts.pop();
            }

            jolts
        })
        .into_iter()
        .fold(0, |n, u| n * 10 + u)
}

fn main() {
    let file = "./inputs/2025-12-03.txt";
    //let file = "./inputs/example.txt";

    let input = read_to_string(file).unwrap();

    let part1: i64 = input.lines().map(|line| joltage(line, 2)).sum();
    println!("part1: {part1}");

    let part2: i64 = input.lines().map(|line| joltage(line, 12)).sum();
    println!("part2: {part2}");
}
