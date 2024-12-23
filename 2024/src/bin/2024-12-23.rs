use hashbrown::{HashMap, HashSet};
use std::collections::VecDeque;
use std::fs::read_to_string;

type Network<'a> = HashMap<&'a str, HashSet<&'a str>>;

fn from(input: &str) -> Network<'_> {
    let mut network: Network = Default::default();

    input.lines().for_each(|line| {
        let (c1, c2) = line.split_once("-").unwrap();
        network
            .entry(c1)
            .and_modify(|s| {
                s.insert(c2);
            })
            .or_insert(HashSet::from([c2]));
        network
            .entry(c2)
            .and_modify(|s| {
                s.insert(c1);
            })
            .or_insert(HashSet::from([c1]));
    });
    network
}

fn trios(network: &Network, chr: char, len: usize) -> usize {
    let t_strings = network
        .keys()
        .filter(|k| k.starts_with(chr))
        .copied()
        .collect::<Vec<_>>();
    let mut queue: VecDeque<(&str, Vec<&str>)> =
        VecDeque::from_iter(t_strings.iter().map(|&t| (t, Vec::from([t]))));

    let mut result: HashSet<Vec<&str>> = Default::default();
    while let Some((comp, net)) = queue.pop_front() {
        if net.len() > len {
            break;
        }
        if net.len() == len {
            if network.get(net[len - 1]).unwrap().contains(net[0]) {
                // println!("{:?}", net);
                let mut net = net.clone();
                net.sort();
                result.insert(net);
            }
            continue;
        }
        for next in network.get(comp).unwrap() {
            if !net.contains(next) {
                let mut nxt_net = net.clone();
                nxt_net.push(*next);
                queue.push_back((*next, nxt_net));
            }
        }
    }

    //println!("{:?}", result);
    result.len()
}

fn main() {
    let file = "./inputs/test.txt";
    // let file = "./inputs/2024-12-23.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let network = from(input.as_str());
    println!("part1: {}", trios(&network, 't', 3));
}
