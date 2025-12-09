use std::cmp::{max, min};
use std::fs::read_to_string;

type Point = (i64, i64);
type Edge = (i64, i64, i64); // (i, jmin, jmax)

fn get_edges(pts: &[Point]) -> (Vec<Edge>, Vec<Edge>) {
    let mut iedges = Vec::new();
    let mut jedges = Vec::new();
    for (i, pt) in pts.iter().enumerate() {
        let next = if i < pts.len() - 1 {
            pts[i + 1]
        } else {
            pts[0]
        };
        if pt.0 == next.0 {
            iedges.push((pt.0, min(pt.1, next.1), max(pt.1, next.1)));
        } else {
            jedges.push((pt.1, min(pt.0, next.0), max(pt.0, next.0)));
        }
    }
    iedges.sort();
    jedges.sort();
    (iedges, jedges)
}

/*
This should be needed IMHO,
but isnt in either part1 or part2

fn is_interior(pt: &Point, edges: &[Edge]) -> bool {
    !edges
        .iter()
        .filter(|e| e.3)
        .take_while(|e| e.0 <= pt.0)
        .filter(|(_, jmin, jmax, _)| *jmin <= pt.1 && pt.1 <= *jmax)
        .count()
        .is_multiple_of(2)
}
*/

fn skip(target: i64, edges: &[Edge]) -> usize {
    let mut range = (0, edges.len());
    while range.0 + 1 < range.1 {
        let mid = (range.0 + range.1) / 2;
        if edges[mid].0 <= target {
            range.0 = mid;
        } else {
            range.1 = mid;
        }
    }
    range.0
}

fn intersect_edges(irange: (i64,i64), jrange:(i64,i64), iedges:&[Edge]) -> bool{
    let iskip = skip(irange.0, iedges);
    iedges
        .iter()
        .skip(iskip)
        .take_while(|e| e.0 <= irange.1)
        .filter(|e| e.0 > irange.0 && e.0 < irange.1)
        .all(|e| e.2 <= jrange.0 || e.1 >= jrange.1)
}

fn no_intersections(start: &Point, end: &Point, iedges: &[Edge], jedges: &[Edge]) -> bool {
    let irange = (min(start.0, end.0), max(start.0, end.0));
    let jrange = (min(start.1, end.1), max(start.1, end.1));

    if !intersect_edges(irange, jrange, iedges){
        return false;
    }
    intersect_edges(jrange, irange, jedges)
}

fn combi(input: &str) -> (u64, u64) {
    let pts: Vec<Point> = input
        .lines()
        .map(|pt| {
            let (i, j) = pt.split_once(',').unwrap();
            (i.parse::<i64>().unwrap(), j.parse::<i64>().unwrap())
        })
        .collect();
    let edges = get_edges(&pts);
    //println!("{:?}", edges);

    let mut max_size = (0, 0);
    for (i, start) in pts.iter().enumerate() {
        for end in pts.iter().skip(i + 1) {
            let size = (1 + start.0.abs_diff(end.0)) * (1 + start.1.abs_diff(end.1));
            max_size.0 = max(max_size.0, size);

            /*
            let centre = ((start.0 + end.0) / 2, (start.1 + end.1) / 2);
            if is_interior(&centre, &edges) && no_intersections(start, end, &edges) {
                max_size.1 = size;
            }
            */
            if size > max_size.1 && no_intersections(start, end, &edges.0, &edges.1) {
                max_size.1 = size;
            }
        }
    }
    max_size
}

fn main() {
    // let input = read_to_string("./inputs/example.txt").unwrap();
    let input = read_to_string("./inputs/2025-12-09.txt").unwrap();

    println!("combi: {:?}", combi(&input));
}
