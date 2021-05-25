use std::fs;

fn mass_fuel(mass: i32) -> i32 {
    let f = mass / 3;
    return if f > 1 { f - 2 } else { 0 };
}

fn fuel(mass: i32) -> i32 {
    let mut m = mass;
    let mut total_fuel = 0;

    loop {
        let f = mass_fuel(m);
        if f == 0 {
            break total_fuel;
        }

        total_fuel += f;
        m = f;
    }
}

pub fn main() {
    let contents = fs::read_to_string("data/day1.txt")
        .expect("Something went wrong reading the file");
    let mut total_fuel: i32 = 0;

    for line in contents.lines() {
        let mass: i32 = line.parse().unwrap();
        total_fuel += fuel(mass);
    }

    println!("Total fuel: {}", total_fuel);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(mass_fuel(12), 2);
        assert_eq!(mass_fuel(14), 2);
        assert_eq!(mass_fuel(2), 0);
        assert_eq!(mass_fuel(1969), 654);
        assert_eq!(mass_fuel(100756), 33583);
        assert_eq!(fuel(14), 2);
        assert_eq!(fuel(1969), 966);
        assert_eq!(fuel(100756), 50346);
    }
}
