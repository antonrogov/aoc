fn process(source: &Vec<usize>, p1: usize, p2: usize) -> Vec<usize> {
    let mut code = source.to_vec();

    code[1] = p1;
    code[2] = p2;

    let mut pos = 0;

    loop {
        let op = code[pos];

        match op {
            1 => {
                let a = code[pos + 1];
                let b = code[pos + 2];
                let r = code[pos + 3];
                code[r] = code[a] + code[b];
            },
            2 => {
                let a = code[pos + 1];
                let b = code[pos + 2];
                let r = code[pos + 3];
                code[r] = code[a] * code[b];
            },
            99 => break code,
            _ => panic!("Invalid op {}", op)
        }

        pos += 4;
    }
}

fn reverse(source: &Vec<usize>, result: usize) -> (usize, usize) {
    for noun in 0..99 {
        for verb in 0..99 {
            if process(source, noun, verb)[0] == result {
                return (noun, verb);
            }
        }
    }

    panic!("Can't determine noun and verb");
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_process() {
        assert_eq!(process(&vec![1,0,0,0,99], 0, 0), [2,0,0,0,99]);
        assert_eq!(process(&vec![2,3,0,3,99], 3, 0), [2,3,0,6,99]);
        assert_eq!(process(&vec![2,4,4,5,99,0], 4, 4), [2,4,4,5,99,9801]);
        assert_eq!(process(&vec![1,1,1,4,99,5,6,0,99], 1, 1), [30,1,1,4,2,5,6,0,99]);
    }

    #[test]
    fn test_main() {
        let contents = fs::read_to_string("data/day2.txt")
            .expect("Something went wrong reading the file");
        let code: Vec<usize> = contents.split(',')
            .map(|s| s.trim().parse().unwrap())
            .collect();

        assert_eq!(process(&code, 12, 2)[0], 9706670);

        assert_eq!(reverse(&code, 19690720), (25, 52));
    }
}
