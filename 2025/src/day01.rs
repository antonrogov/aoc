use std::fs::read_to_string;

fn main() {
    let contents = read_to_string("data/day01.txt").unwrap();
    let vs: Vec<(i16, i16)> = contents.lines().map(|l| {
        let dir = l.chars().next().unwrap();
        let n = l[1..].parse().ok().unwrap();
        (if dir == 'L' { -1 } else { 1 }, n)
    }).collect();

    let res = vs.iter().fold((50, 0, 0), |(v, c, bc), (d, by)| {
        let mut c1 = c + by / 100;
        let mut v1 = v + d * (by % 100);

        if v1 == 0 && v != 0 {
            c1 += 1;
        } else if v1 < 0 {
            v1 += 100;
            if v != 0 { c1 += 1 }
        } else if v1 >= 100 {
            v1 -= 100;
            c1 += 1;
        }
        (v1, c1, bc + by / 100)
    });

    println!("{res:?}");
}
