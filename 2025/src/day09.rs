use std::fs::read_to_string;

type Point = (u64, u64);
type Edge<'a> = (&'a Point, &'a Point);

fn within((x, y): &Point, lines: &[Vec<(u64, u64)>]) -> bool {
    match lines.get(*y as usize) {
        Some(spans) => spans.iter().any(|(x1, x2)| x1 <= x && x <= x2),
        None => false
    }
}

fn rect_within((x1, y1): &Point, (x2, y2): &Point, lines: &[Vec<(u64, u64)>]) -> bool {
    for x in *x1.min(x2)..=*x1.max(x2) {
        if !within(&(x, *y1), lines) { return false }
        if !within(&(x, *y2), lines) { return false }
    }
    for y in *y1.min(y2)..=*y1.max(y2) {
        if !within(&(*x1, y), lines) { return false }
        if !within(&(*x2, y), lines) { return false }
    }
    true
}

fn main() {
    let contents = read_to_string("data/day09.txt").unwrap();
    let points: Vec<Point> = contents.lines().map(|l| {
        let mut ns = l.split(",");
        (ns.next().unwrap().parse().ok().unwrap(), ns.next().unwrap().parse().ok().unwrap())
    }).collect();

    let mut edges: Vec<Edge> = Vec::new();
    for i in 0..points.len() {
        let p1 = points.get(i).unwrap();
        let p2 = points.get((i + 1) % points.len()).unwrap();
        edges.push((p1, p2));
    }

    let h = points.iter().map(|p| p.1).max().unwrap();

    let mut lines: Vec<Vec<(u64, u64)>> = Vec::with_capacity(h as usize);
    for y in 0..h {
        let mut xs: Vec<u64> = Vec::new();
        let mut spans: Vec<(u64, u64)> = Vec::new();

        for ((ax, ay), (bx, by)) in edges.iter() {
            if ax == bx {
                if *ay.min(by) <= y && y < *ay.max(by) {
                    xs.push(*ax);
                }
            } else if *ay == y {
                spans.push((*ax.min(bx), *ax.max(bx)));
            }
        }
        xs.sort_unstable();
        spans.extend(xs.chunks_exact(2).map(|s| (s[0], s[1])));

        lines.push(spans);
    }

    let mut area = 0;
    for p1 in points.iter() {
        for p2 in points.iter() {
            if rect_within(p1, p2, &lines) {
                let a = (p1.0.abs_diff(p2.0) + 1) * (p1.1.abs_diff(p2.1) + 1);
                if a > area { area = a }
            }
        }
    }
    println!("{area}");
}
