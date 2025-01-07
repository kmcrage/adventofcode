use hashbrown::HashMap;
use std::fs::read_to_string;

fn from(input: &str) -> (HashMap<String, bool>, Vec<[String; 4]>) {
    let (input_v, input_ops) = input.split_once("\n\n").unwrap();

    let values = input_v
        .lines()
        .map(|l| {
            let pair = l.split_once(": ").unwrap();
            (pair.0.to_string(), pair.1 == "1")
        })
        .collect();
    let ops = input_ops
        .lines()
        .map(|l| {
            let (op, result) = l.split_once(" -> ").unwrap();
            let tokens = op.split(" ").collect::<Vec<_>>();
            [
                tokens[0].to_string(),
                tokens[1].to_string(),
                tokens[2].to_string(),
                result.to_string(),
            ]
        })
        .collect();

    (values, ops)
}

fn compute_map(values: &HashMap<String, bool>, ops: &[[String; 4]]) -> HashMap<String, bool> {
    let mut vs = values.clone();
    let mut to_eval = ops.to_vec();
    while !to_eval.is_empty() {
        let mut next_eval: Vec<[String; 4]> = Default::default();
        to_eval.iter().for_each(|eqn| {
            if vs.contains_key(&eqn[0]) && vs.contains_key(&eqn[2]) {
                match eqn[1].as_str() {
                    "AND" => {
                        vs.insert(eqn[3].clone(), vs[&eqn[0]] & vs[&eqn[2]]);
                    }
                    "XOR" => {
                        vs.insert(eqn[3].clone(), vs[&eqn[0]] ^ vs[&eqn[2]]);
                    }
                    "OR" => {
                        vs.insert(eqn[3].clone(), vs[&eqn[0]] | vs[&eqn[2]]);
                    }
                    _ => panic!(),
                }
            } else {
                next_eval.push(eqn.clone())
            }
        });
        to_eval = next_eval;
    }
    vs
}

fn compute(values: &HashMap<String, bool>, ops: &[[String; 4]]) -> usize {
    let vs = compute_map(values, ops);
    let mut zs: Vec<String> = vs.keys().filter(|k| k.starts_with("z")).cloned().collect();
    zs.sort();
    zs.reverse();
    zs.iter()
        .fold(0, |a, z| 2 * a + (if vs[z] { 1 } else { 0 }))
}

fn get_num(input: &str) -> String {
    let mut num = input.split("-").next().unwrap().chars();
    num.next();
    num.as_str().to_string()
}

fn do_corrections(ops: &[[String; 4]], corrections: &[(&str, &str)]) -> Vec<[String; 4]> {
    let corrections: HashMap<&str, &str> = corrections
        .iter()
        .flat_map(|(a, b)| [(*b, *a), (*a, *b)])
        .collect();

    ops.iter()
        .map(|op| {
            let mut newop = op.clone();
            if corrections.contains_key(newop[3].as_str()) {
                newop[3] = corrections[newop[3].as_str()].to_string();
            }
            newop
        })
        .collect()
}

fn out_rule(
    in1: &str,
    in2: &str,
    action: &str,
    out: &str,
    op: &[String; 4],
    rename: &mut HashMap<String, String>,
) {
    if !op[3].contains("-")
        && ((op[0].starts_with(in1) && op[2].starts_with(in2))
            || (op[0].starts_with(in2) && op[2].starts_with(in1)))
        && op[1] == action
    {
        rename.insert(
            op[3].clone(),
            format!("{}{}-{}", out, get_num(&op[0]), op[3]).to_string(),
        );
    }
}

fn in_rule(
    in1: &str,
    out: &str,
    action: &str,
    in2: &str,
    op: &[String; 4],
    rename: &mut HashMap<String, String>,
) {
    for i in [0, 2] {
        if op[i].starts_with(in1)
            && op[1] == action
            && !op[2 - i].contains("-")
            && op[3].starts_with(out)
        {
            rename.insert(
                op[2 - i].clone(),
                format!("{}{}-{}", in2, get_num(&op[0]), op[2 - i]).to_string(),
            );
        }
    }
}

fn apply_rules(op: &[String; 4], rename: &HashMap<String, String>) -> [String; 4] {
    let mut newop = op.clone();
    for i in [0, 2, 3] {
        if let Some(r) = rename.get(&op[i]) {
            newop[i] = r.to_string();
        }
    }
    newop
}

fn fix(ops: &[[String; 4]]) -> String {
    let corrections = [
        // the z corrections are easily found by inspection
        ("shh", "z21"),
        ("dqr", "z33"),
        ("pfw", "z39"),
        // this one harder but its obviously in 25 - 27
        ("vgs", "dtk"),
    ];
    
    let mut prev: Vec<[String; 4]> = Default::default();
    let mut curr = do_corrections(ops, &corrections);

    let mut rename: HashMap<String, String> = Default::default();
    while prev != curr {
        prev = curr;
        curr = Default::default();
        for op in &prev {
            // create new renamings from rules
            out_rule("x", "y", "XOR", "L", op, &mut rename);
            out_rule("x", "y", "AND", "M", op, &mut rename);
            out_rule("L", "N", "AND", "P", op, &mut rename);
            in_rule("L", "z", "XOR", "N", op, &mut rename);
            // apply existing renames
            let newop = apply_rules(op, &rename);
            curr.push(newop);
        }
    }

    curr.sort();
    for op in curr {
        println!("{:?}", op);
    }

    let mut result = corrections
        .iter()
        .flat_map(|(a, b)| [a.to_string(), b.to_string()])
        .collect::<Vec<_>>();
    result.sort();
    result.join(",")
}

fn main() {
    //let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-24.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let (values, ops) = from(input.as_str());
    println!("part1: {}", compute(&values, &ops));
    println!("part2: {}", fix(&ops));
}
