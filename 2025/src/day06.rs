use std::fs::read_to_string;

fn calc(vals: &[i64], op: char) -> i64 {
    if op == '+' {
        vals.iter().sum()
    } else {
        vals.iter().product()
    }
}

fn main() {
    let contents = read_to_string("data/day06.txt").unwrap();
    let data: Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect()).collect();
    let mut sum = 0;

    // for (i, _) in data.first().unwrap().iter().enumerate() {
    //     let mut iter = data.iter().enumerate().rev();
    //     let op = iter.next().unwrap().1.get(i).unwrap().chars().next().unwrap();
    //     let mut res = if op == '+' { 0 } else { 1 };
    //     for (j, line) in iter {
    //         let val: i64 = line.get(i).unwrap().parse().ok().unwrap();
    //         res = if op == '+' { res + val } else { res * val };
    //     }
    //     sum += res;
    // }

    let w = data.first().unwrap().len();
    let h = data.len();
    let mut vals: Vec<i64> = Vec::new();
    let mut op = '+';
    for x in 0..w {
        let mut m = String::new();
        for y in 0..h - 1 {
            m.push(*data.get(y).unwrap().get(x).unwrap());
        }
        let last = *data.get(h - 1).unwrap().get(x).unwrap();

        if last == ' ' && m.chars().all(|c| c == ' ') {
            sum += calc(&vals, op);
            vals.clear();
        } else {
            if last != ' ' { op = last }
            vals.push(m.trim().parse().ok().unwrap());
        }
    }
    sum += calc(&vals, op);

    println!("{sum}");
}
