use std::fs::read_to_string;

fn main() {
    let contents = read_to_string("data/day05.txt").unwrap();
    let mut ranges: Vec<(u64, u64)> = Vec::new();
    let mut combined: Vec<(u64, u64)> = Vec::new();
    let mut count = 0;
    for line in contents.lines() {
        if line.is_empty() {
            ranges.sort_by(|a, b| a.0.cmp(&b.0));
            for r in ranges.iter() {
                if combined.is_empty() || combined.last().unwrap().1 < r.0 {
                    combined.push(*r);
                } else {
                    let last = combined.last_mut().unwrap();
                    if last.1 < r.1 { last.1 = r.1 }
                }
            }
            println!("{}", combined.iter().fold(0, |acc, (a,b)| acc + b - a + 1));
            return;
        } else if combined.is_empty() {
            let mut parts = line.split("-");
            let a = parts.next().unwrap().parse().ok().unwrap();
            let b = parts.next().unwrap().parse().ok().unwrap();
            ranges.push((a, b));
        } else {
            let id: u64 = line.parse().ok().unwrap();
            if combined.iter().any(|r| r.0 <= id && id <= r.1) {
                count += 1;
            }
        }
    }

    println!("{count}");
}
