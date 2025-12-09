use std::fs::read_to_string;

#[derive(Debug)]
struct Field {
    cells: Vec<String>
}

impl Field {
    fn get(self: &Field, x: i32, y: i32) -> Option<char> {
        if x < 0 || y < 0 {
            None
        } else {
            match self.cells.get(y as usize) {
                Some(line) => line.chars().nth(x as usize),
                None => None
            }
        }
    }

    fn neighbors(self: &Field, x: i32, y: i32) -> [Option<char>; 8] {
        let mut res = [None; 8];
        let mut i = 0;
        for dy in 0..=2 {
            for dx in 0..=2 {
                if dy != 1 || dx != 1 {
                    res[i] = self.get(x + dx - 1, y + dy - 1);
                    i += 1;
                }
            }
        }
        res
    }

    fn can_remove(self: &Field, x: i32, y: i32) -> bool {
       self.neighbors(x, y).iter().filter(|c| c.is_some_and(|c| c == '@')).count() < 4
    }
}

fn main() {
    let contents = read_to_string("data/day04.txt").unwrap();
    let mut field = Field {
        cells: contents.lines().map(|l| l.chars().collect()).collect()
    };

    let mut removed = 0;
    loop {
        let mut removed_now = 0;
        let new_field = Field {
            cells: field.cells.iter().enumerate().map(|(y, line)| {
               line.chars().enumerate().map(|(x, c)| {
                   if c == '@' && field.can_remove(x as i32, y as i32) {
                       removed_now += 1;
                       '.'
                   } else {
                       c
                   }
               }).collect()
            }).collect()
        };

        removed += removed_now;
        if removed_now == 0 {
            break;
        }

        field = new_field;
    }

    println!("{removed}");
}
