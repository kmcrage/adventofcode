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

fn compute_map(values: &HashMap<String, bool>, ops: &Vec<[String; 4]>) -> HashMap<String, bool> {
    let mut vs = values.clone();
    let mut to_eval = ops.clone();
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

fn compute(values: &HashMap<String, bool>, ops: &Vec<[String; 4]>) -> usize {
    let vs = compute_map(values, ops);
    let mut zs: Vec<String> = vs.keys().filter(|k| k.starts_with("z")).cloned().collect();
    zs.sort();
    zs.reverse();
    zs.iter()
        .fold(0, |a, z| 2 * a + (if vs[z] { 1 } else { 0 }))
}

fn get_num(input: &String) -> String {
    let mut num = input.split("-").next().unwrap().chars();
    num.next();
    num.as_str().to_string()
}

fn fix(ops: &[[String; 4]]) -> usize {
    let mut curr: Vec<[String; 4]> = ops.to_vec();
    let mut prev: Vec<[String; 4]> = Default::default();
    let mut rename: HashMap<String, String> = Default::default();
    while prev != curr {
        prev = curr;
        curr = Default::default();
        for op in &prev {
            let mut newop = op.clone();
            // rename rules
            if !op[3].contains("-")
                && ((op[0].starts_with("x") && op[2].starts_with("y"))
                    || (op[0].starts_with("y") && op[2].starts_with("x")))
            {
                if op[1] == "XOR" {
                    rename.insert(
                        op[3].clone(),
                        format!("L{}-{}", get_num(&op[0]), op[3]).to_string(),
                    );
                }
                if op[1] == "AND" {
                    rename.insert(
                        op[3].clone(),
                        format!("M{}-{}", get_num(&op[0]), op[3]).to_string(),
                    );
                }
            }
            if ((op[0].starts_with("P") && op[2].starts_with("M"))
                || (op[0].starts_with("M") && op[2].starts_with("P")))
                && op[1] == "OR"
                && !op[3].contains("-")
            {
                rename.insert(
                    op[3].clone(),
                    format!("C{}-{}", get_num(&op[0]), op[3]).to_string(),
                );
            }
            if op[0].starts_with("L")
                && op[1] == "XOR"
                && !op[2].contains("-")
                && op[3].starts_with("z")
            {
                rename.insert(
                    op[2].clone(),
                    format!("N{}-{}", get_num(&op[0]), op[2]).to_string(),
                );
            }
            if op[2].starts_with("L")
                && op[1] == "XOR"
                && !op[0].contains("-")
                && op[3].starts_with("z")
            {
                rename.insert(
                    op[0].clone(),
                    format!("N{}-{}", get_num(&op[2]), op[0]).to_string(),
                );
            }
            if !op[3].contains("-")
                && ((op[0].starts_with("L") && op[2].starts_with("N"))
                    || (op[0].starts_with("N") && op[2].starts_with("L")))
            {
                if op[1] == "AND" {
                    rename.insert(
                        op[3].clone(),
                        format!("P{}-{}", get_num(&op[0]), op[3]).to_string(),
                    );
                }
            }
            //
            for i in [0, 2, 3] {
                if let Some(r) = rename.get(&op[i]) {
                    newop[i] = r.to_string();
                }
            }
            curr.push(newop);
        }
    }

    curr.sort();
    for op in curr {
        println!("{:?}", op);
    }
    0
}

fn main() {
    //let file = "./inputs/test.txt";
    let file = "./inputs/2024-12-24.txt";
    let input = read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {}", file));
    let (values, ops) = from(input.as_str());
    println!("part1: {}", compute(&values, &ops));
    println!("part2: {}", fix(&ops));
}
// dqr,dtk,pfw,shh,vgs,z21,z33,z39