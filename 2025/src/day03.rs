use std::fs::read_to_string;

fn check(s: &str, n: u32) -> u64 {
    let head = &s[..s.len() - n as usize + 1];
    let max_c = head.chars().max().unwrap();
    let max = max_c.to_digit(10).unwrap() as u64;
    if n == 1 {
        return max;
    }

    let mut res = 0;
    for (i, c) in head.chars().enumerate() {
        if c == max_c {
            let rest = check(&s[i + 1..], n - 1);
            let r = max * 10u64.pow(n - 1) + rest;
            if r > res { res = r }
        }
    }
    res
}

fn main() {
    let contents = read_to_string("data/day03.txt").unwrap();
    let mut sum = 0;
    for line in contents.lines() {
        sum += check(line, 12);
    }
    println!("{sum}");
}
