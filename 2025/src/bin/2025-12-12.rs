use std::fs::read_to_string;

fn part1(input: &str) -> usize {
    let sections = input.split("\n\n").collect::<Vec<_>>();
    let sizes = sections
        .iter()
        .map(|s| s.chars().filter(|c| c == &'#').count())
        .collect::<Vec<_>>();
    sections[sections.len() - 1]
        .lines()
        .filter(|p| {
            let (grid, counts) = p.split_once(": ").unwrap();
            let (width, height) = grid.split_once('x').unwrap();
            let (width, height): (usize, usize) = (width.parse().unwrap(), height.parse().unwrap());
            let tile_area = counts
                .split_whitespace()
                .map(|c| c.parse::<usize>().unwrap())
                .zip(sizes.clone())
                .map(|(count, size)| size * count)
                .sum::<usize>();

            tile_area < width * height
        })
        .count()
}

fn main() {
    //let input = read_to_string("./inputs/example.txt").unwrap();
    let input = read_to_string("./inputs/2025-12-12.txt").unwrap();

    println!("part1: {:?}", part1(&input));
}
