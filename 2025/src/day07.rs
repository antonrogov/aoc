use std::fs::read_to_string;

type Beam = (usize, usize);

fn add(beams: &mut Vec<Beam>, i: usize, c: usize) {
    match beams.iter_mut().find(|b| b.0 == i) {
        Some(b) => b.1 += c,
        None => beams.push((i, c))
    }
}

fn main() {
    let contents = read_to_string("data/day07.txt").unwrap();
    let mut beams: Vec<Beam> = Vec::new();
    let mut splits = 0;
    for line in contents.lines() {
        if beams.is_empty() {
            beams.push((line.find('S').unwrap(), 1));
        } else {
            let mut new_beams: Vec<Beam> = Vec::new();
            for (i, c) in beams {
                if line.chars().nth(i).unwrap() == '^' {
                    splits += 1;
                    if i > 0 {
                        add(&mut new_beams, i - 1, c);
                    }
                    if i < line.len() - 1 {
                        add(&mut new_beams, i + 1, c);
                    }
                } else {
                    add(&mut new_beams, i, c);
                }
            }
            beams = new_beams;
        }
        println!("{} {}", splits, beams.iter().fold(0, |s, (_, c)| s + c));
    }
}
