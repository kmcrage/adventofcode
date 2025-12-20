use binary_heap_plus::{BinaryHeap, MinComparator};
// use hashbrown::{HashMap, HashSet};
use itertools::Itertools;
use std::fs::read_to_string;

type Point = (i64, i64, i64);
type Pairs = BinaryHeap<(u64, usize, usize), MinComparator>;

#[derive(Clone, Debug)]
struct UnionFindNode {
    point: Point,
    parent: usize,
    size: usize,
}

#[derive(Clone, Debug)]
struct UnionFind {
    forest: Vec<UnionFindNode>,
}

impl UnionFind {
    fn find(&mut self, n: usize) -> usize {
        let mut x = n;
        while self.forest[x].parent != x {
            (x, self.forest[x].parent) = (
                self.forest[x].parent,
                self.forest[self.forest[x].parent].parent,
            );
        }
        x
    }

    fn union(&mut self, x: usize, y: usize) {
        let mut x = self.find(x);
        let mut y = self.find(y);

        if x == y {
            return;
        }

        // If necessary, swap variables to ensure that
        // x has at least as many descendants as y
        if self.forest[x].size < self.forest[y].size {
            (x, y) = (y, x);
        }

        // Make x the new root
        self.forest[y].parent = x;
        // Update the size of x
        self.forest[x].size += self.forest[y].size;
    }

    fn areas(&self) -> BinaryHeap<usize> {
        let areas = self.forest
            .iter()
            .enumerate()
            .filter(|(i, node)| *i == node.parent)
            .map(|(_, node)| node.size)
            .collect::<Vec<_>>();
        BinaryHeap::from_vec(areas)
    }
}

fn dist(x: &Point, y: &Point) -> u64 {
    x.0.abs_diff(y.0).pow(2) + x.1.abs_diff(y.1).pow(2) + x.2.abs_diff(y.2).pow(2)
}

fn parse(input: &str) -> (UnionFind, Pairs) {
    let junctions: Vec<UnionFindNode> = input
        .lines()
        .flat_map(|line| {
            line.split(',')
                .map(|n| n.parse::<i64>().unwrap())
                .tuples::<(_, _, _)>()
        })
        .enumerate()
        .map(|(i, pt)| UnionFindNode {
            point: pt,
            parent: i,
            size: 1,
        })
        .collect();

    let pairs = junctions
        .iter()
        .enumerate()
        .flat_map(|(i, start)| {
            junctions
                .iter()
                .enumerate()
                .take(i)
                .map(|(j, end)| (dist(&start.point, &end.point), i, j))
                .collect::<Vec<_>>()
        })
        .collect();

    (UnionFind { forest: junctions }, BinaryHeap::from_vec(pairs))
}

fn part1(uf: &mut UnionFind, pairs: &Pairs, num: usize) -> usize {
    for (_, start, end) in pairs.clone().into_iter_sorted().take(num) {
        uf.union(start, end)
    }

    uf.areas().into_iter_sorted().take(3).product()
}

fn part2(uf: &mut UnionFind, pairs: &Pairs) -> i64 {
    let mut pairs = pairs.clone();
    while let Some((_, start, end)) = pairs.pop() {
        uf.union(start, end);

        let root = uf.find(0);
        if uf.forest[root].size == uf.forest.len(){
            return uf.forest[start].point.0 * uf.forest[end].point.0;
        }   
    }
    0
}


fn main() {
    //let input = read_to_string("./inputs/example.txt").unwrap();
    let input = read_to_string("./inputs/2025-12-08.txt").unwrap();
    let (mut uf, pairs) = parse(&input);

    println!("part1: {}", part1(&mut uf, &pairs, 1000));
    println!("part2: {}", part2(&mut uf, &pairs));
}
