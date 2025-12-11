use std::fs::read_to_string;

type Coord = Vec<i64>;
type Box = (usize, Coord, Vec<usize>);
type Link = (usize, (usize, u64, usize), usize);

fn dist(b1: &Coord, b2: &Coord) -> u64 {
    b1.iter().zip(b2).fold(0, |d, (c1, c2)| d + (c1 - c2).pow(2) as u64)
}

fn connect(boxes: &mut [Box], dists: &[Vec<u64>]) -> Option<(usize, usize)> {
    let links: Vec<Link> = boxes.iter().enumerate().map(|(i, b1)| {
        (i, boxes.iter().enumerate()
                // .filter(|(j, _)| !b1.2.contains(j))
                .filter(|(_, b2)| b2.0 != b1.0)
                .map(|(j, b2)| (j, *dists.get(i).unwrap().get(j).unwrap(), b2.0))
                .min_by_key(|(_, d, _)| *d).unwrap(), b1.0)
    }).collect();

    let (i, (j, _, id2), id1) = links.iter()
        .filter(|(i, (j, _, _), _)| links.get(*j).unwrap().1.0 == *i)
        .min_by_key(|(_, (_, d, _), _)| *d).unwrap();

    let id = id1.min(id2);
    let mut connected = true;

    for (k, b) in boxes.iter_mut().enumerate() {
        if k == *i {
            b.2.push(*j)
        } else if k == *j {
            b.2.push(*i)
        }
        if b.0 == *id1 || b.0 == *id2 {
            b.0 = *id
        } else {
            connected = false;
        }
    }

    if connected { Some((*i, *j)) } else { None }
}

fn main() {
    let contents = read_to_string("data/day08.txt").unwrap();
    let mut boxes: Vec<Box> = contents.lines().enumerate().map(|(i, l)|
        (i, l.split(",").map(|s| s.parse().ok().unwrap()).collect(), vec![i])
    ).collect();

    let dists: Vec<Vec<u64>> = boxes.iter().map(|b1|
        boxes.iter().map(|b2| dist(&b1.1, &b2.1)).collect()
    ).collect();

    // for _ in 0..1000 {
    //     connect(&mut boxes, &dists);
    // }
    //
    // let mut map: Vec<(usize, i64)> = Vec::new();
    // for b in boxes.iter() {
    //     match map.iter().position(|m| m.0 == b.0) {
    //         Some(i) => map.get_mut(i).unwrap().1 += 1,
    //         None => map.push((b.0, 1))
    //     }
    // }
    // map.sort_by_key(|m| -m.1);
    // println!("{}", map.first_chunk::<3>().unwrap().iter().fold(1, |p, m| p * m.1));

    loop {
        if let Some((i, j)) = connect(&mut boxes, &dists) {
            let x1 = boxes.get(i).unwrap().1.first().unwrap();
            let x2 = boxes.get(j).unwrap().1.first().unwrap();
            println!("{}", x1 * x2);
            break
        }
    }
}
