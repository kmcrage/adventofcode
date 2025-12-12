use std::fs::read_to_string;

fn main() {
    let input = read_to_string("./inputs/2025-12-12.txt").unwrap();
    // let input = read_to_string("./inputs/example.txt").unwrap();

    let sections = input.split("\n\n").collect::<Vec<_>>();
    let tile_size = sections[0].lines().count() - 1;
    let sizes = sections
        .iter()
        .map(|s| s.chars().filter(|c| c == &'#').count())
        .collect::<Vec<_>>();

    let flags = sections[sections.len() - 1]
        .lines()
        .map(|p| {
            let (grid, counts) = p.split_once(": ").unwrap();
            let (width, height) = grid.split_once('x').unwrap();
            let (width, height): (usize, usize) = (width.parse().unwrap(), height.parse().unwrap());

            let tile_area = counts
                .split_whitespace()
                .map(|c| c.parse::<usize>().unwrap())
                .zip(sizes.clone())
                .map(|(count, size)| size * count)
                .sum::<usize>();
            let num_tiles = counts
                .split_whitespace()
                .map(|c| c.parse::<usize>().unwrap())
                .sum();

            (
                width * height > tile_area,
                (width / tile_size) * (height / tile_size) >= num_tiles
            )
        })
        .collect::<Vec<_>>();
    println!("Possibles: {}", flags.iter().filter(|f| f.0).count());
    println!("Definites: {}", flags.iter().filter(|f| f.1).count());
    println!("Hard: {}", flags.iter().filter(|f| f.0 && !f.1).count());
}
