use hashbrown::HashMap;
use std::fs::read_to_string;

type Connections<'a> = HashMap<&'a str, Vec<&'a str>>;
type Cache = HashMap<(String, bool, bool), u64>;

#[derive(Clone, Debug)]
struct State<'a> {
    location: &'a str,
    seen_fft: bool,
    seen_dac: bool,
}

fn count_paths(
    here: &State,
    target: &str,
    connections: &Connections,
    cache: &mut Cache,
    is_part2: bool,
) -> u64 {
    let key = (here.location.to_string(), here.seen_dac, here.seen_fft);
    if let Some(result) = cache.get(&key) {
        return *result;
    }

    let mut result = 0;
    for nxt in &connections[here.location] {
        if *nxt == target {
            if !is_part2 || (here.seen_dac && here.seen_fft) {
                result += 1;
            }
            continue;
        }

        let mut next = State {
            location: nxt,
            ..*here
        };
        if is_part2 {
            match *nxt {
                "dac" => next.seen_dac = true,
                "fft" => next.seen_fft = true,
                _ => (),
            }
        }

        result += count_paths(&next, target, connections, cache, is_part2);
    }
    cache.insert(key, result);
    result
}

fn combi(start: &str, target: &str, input: &str, is_part2: bool) -> u64 {
    let connections: Connections = input
        .lines()
        .map(|line| {
            let (input, outputs) = line.split_once(": ").unwrap();
            (input, outputs.split_whitespace().collect())
        })
        .collect();

    let mut cache = HashMap::new();
    let current = State {
        location: start,
        seen_dac: false,
        seen_fft: false,
    };
    count_paths(&current, target, &connections, &mut cache, is_part2)
}

fn main() {
    // let input = read_to_string("./inputs/example.txt").unwrap();
    let input = read_to_string("./inputs/2025-12-11.txt").unwrap();

    println!("part1: {:?}", combi("you", "out", &input, false));
    println!("part2: {:?}", combi("svr", "out", &input, true));
}
