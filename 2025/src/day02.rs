use std::fs::read_to_string;

fn is_invalid(n: i64) -> bool {
    let mut a: i64 = 10;
    while a * a / 10 < n {
        let p = n % a;
        let mut n2 = n / a;
        while n2 % a == p {
            if n2 == p && n2 >= a / 10 { return true }
            n2 /= a;
        }
        a = a * 10;
    }
    false
}

fn main() {
    let contents = read_to_string("data/day02.txt").unwrap();
    let mut sum = 0;
    for range in contents.strip_suffix("\n").unwrap().split(",") {
        let ns: Vec<i64> = range.split("-").map(|n| n.parse().ok().unwrap()).collect();
        for n in *ns.first().unwrap()..=*ns.last().unwrap() {
            if is_invalid(n) {
                sum += n
            }
        }
    }
    println!("{sum}");
}
