use binary_heap_plus::{BinaryHeap, MinComparator};
use hashbrown::{HashMap, HashSet};
use itertools::Itertools;
use std::fs::read_to_string;

type Point = (i64, i64, i64);
type Pairs = BinaryHeap<(i64, Point, Point), MinComparator>;

fn dist(x: &Point, y: &Point) -> i64 {
    (x.0 - y.0) * (x.0 - y.0) + (x.1 - y.1) * (x.1 - y.1) + (x.2 - y.2) * (x.2 - y.2)
}

fn part1(pairs: &Pairs, num: usize) -> usize {
    let mut connections: HashMap<Point, Vec<Point>> = HashMap::new();

    for (_, start, end) in pairs.clone().into_iter_sorted().take(num) {
        if let Some(v) = connections.get_mut(&start) {
            v.push(end);
        } else {
            let v = vec![end];
            connections.insert(start, v);
        }
        if let Some(v) = connections.get_mut(&end) {
            v.push(start);
        } else {
            let v = vec![start];
            connections.insert(end, v);
        }
    }

    let mut sizes: BinaryHeap<usize> = BinaryHeap::new();
    let mut circuit: HashSet<Point> = HashSet::new();
    let mut queue;
    while !connections.is_empty() {
        queue = vec![*connections.keys().next().unwrap()];
        circuit.clear();
        while let Some(pt) = queue.pop() {
            if circuit.contains(&pt) {
                continue;
            }
            circuit.insert(pt);
            queue.extend(connections.get(&pt).unwrap());
            connections.remove(&pt);
        }
        sizes.push(circuit.len());
    }
    sizes.into_iter_sorted().take(3).product()
}

fn parse(input: &str) -> Pairs {
    let junctions: Vec<Point> = input
        .lines()
        .flat_map(|line| {
            line.split(',')
                .map(|n| n.parse::<i64>().unwrap())
                .tuples::<(_, _, _)>()
        })
        .collect();

    let pairs = junctions
        .iter()
        .enumerate()
        .flat_map(|(i, start)| {
            junctions
                .iter()
                .skip(i + 1)
                .map(|end| (dist(start, end), *start, *end))
                .collect::<Vec<_>>()
        })
        .collect();
    BinaryHeap::from_vec(pairs)
}

fn part2(pairs: &Pairs) -> i64 {
    let pairs_len = pairs.len();
    let mut pairs = pairs.clone();

    let mut connections: HashMap<Point, Vec<Point>> = HashMap::new();
    let mut circuit: HashSet<Point> = HashSet::new();
    while let Some((_, start, end)) = pairs.pop() {
        if let Some(v) = connections.get_mut(&start) {
            v.push(end);
        } else {
            let v = vec![end];
            connections.insert(start, v);
        }
        if let Some(v) = connections.get_mut(&end) {
            v.push(start);
        } else {
            let v = vec![start];
            connections.insert(end, v);
        }

        // this hasn't got enough connections to connect everything
        if connections.len() * (connections.len() - 1) < 2 * pairs_len {
            continue;
        }
        // the new connection doesn't add to the circuit
        if !circuit.is_empty() && !circuit.contains(&start) && !circuit.contains(&end) {
            continue;
        }

        let mut queue = vec![start, end];

        while let Some(pt) = queue.pop() {
            if circuit.contains(&pt) {
                continue;
            }
            circuit.insert(pt);
            queue.extend(connections.get(&pt).unwrap());
        }
        if circuit.len() * (circuit.len() - 1) == 2 * pairs_len {
            return start.0 * end.0;
        }
    }
    0
}

fn main() {
    //let input = read_to_string("./inputs/example.txt").unwrap();
    let input = read_to_string("./inputs/2025-12-08.txt").unwrap();
    let pairs = parse(&input);

    //println!("part1: {}", part1(&pairs, 10));
    //println!("part2: {}", part2(&pairs));

    println!("part1: {}", part1(&pairs, 1000));
    println!("part2: {}", part2(&pairs));
}
