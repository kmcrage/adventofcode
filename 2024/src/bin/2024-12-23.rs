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

/*
https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm

given three disjoint sets of vertices R, P, and X, it finds the
maximal cliques that include all of the vertices in R,
some of the vertices in P, and none of the vertices in X

algorithm BronKerbosch1(R, P, X) is
    if P and X are both empty then
        report R as a maximal clique
    for each vertex v in P do
        BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
        P := P \ {v}
        X := X ⋃ {v}

algorithm BronKerbosch2(R, P, X) is
    if P and X are both empty then
        report R as a maximal clique
    choose a pivot vertex u in P ⋃ X
    for each vertex v in P \ N(u) do
        BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
        P := P \ {v}
        X := X ⋃ {v}
*/

fn bron_kerbosch2<'a>(
    network: &'a Network,
    r: HashSet<&'a str>,
    mut p: HashSet<&'a str>,
    mut x: HashSet<&'a str>,
    cliques: &mut Vec<HashSet<&'a str>>,
) {
    if p.is_empty() {
        if x.is_empty() {
            cliques.push(r);
        }
        return;
    }

    let nodes = p.clone();
    let pivot = *p.union(&x).max_by_key(|&&v| network[v].len()).unwrap();
    nodes.difference(&network[pivot]).for_each(|v| {
        let nhbd_v = network.get(v).unwrap();
        let v_set = HashSet::from([*v]);
        bron_kerbosch2(
            network,
            r.union(&v_set).cloned().collect(),
            p.intersection(nhbd_v).cloned().collect(),
            x.intersection(nhbd_v).cloned().collect(),
            cliques,
        );
        p.remove(v);
        x.insert(*v);
    })
}

fn maximal_clique(network: &Network) -> String {
    let r = HashSet::new();
    let p = HashSet::from_iter(network.keys().cloned());
    let x = HashSet::new();
    let mut cliques: Vec<HashSet<&str>> = Default::default();
    bron_kerbosch2(network, r, p, x, &mut cliques);

    let mut maximal = cliques
        .iter()
        .max_by_key(|c| c.len())
        .unwrap()
        .into_iter()
        .cloned()
        .map(String::from)
        .collect::<Vec<_>>();
    maximal.sort();
    maximal.join(",")
}

fn main() {
    // let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-23.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let network = from(input.as_str());
    println!("part1: {}", trios(&network, 't', 3));
    println!("part2: {:?}", maximal_clique(&network));
}
