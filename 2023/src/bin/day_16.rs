use nom::character::complete as nom_char;
use std::collections::{HashMap, HashSet};

type Pos = (usize, usize);
type Parser<'a, T> = nom::IResult<&'a str, T>;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Dir {
    Up,
    Right,
    Down,
    Left,
}

impl Dir {
    fn step_from(&self, pos: &Pos) -> Option<Pos> {
        match self {
            Dir::Right => Some((pos.0 + 1, pos.1)),
            Dir::Down => Some((pos.0, pos.1 + 1)),
            Dir::Left => {
                let x = pos.0.checked_sub(1)?;
                Some((x, pos.1))
            }
            Dir::Up => {
                let y = pos.1.checked_sub(1)?;
                Some((pos.0, y))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Obj {
    MirrorUpRight,
    MirrorDownRight,
    SplitLeftRight,
    SplitUpDown,
}

impl Obj {
    fn continue_from(&self, from: &Dir) -> Dir {
        match (self, from) {
            (Obj::SplitLeftRight, Dir::Down) => Dir::Left,
            (Obj::SplitLeftRight, Dir::Up) => Dir::Right,
            (Obj::SplitUpDown, Dir::Left) => Dir::Down,
            (Obj::SplitUpDown, Dir::Right) => Dir::Up,
            (Obj::MirrorUpRight, Dir::Down) => Dir::Left,
            (Obj::MirrorUpRight, Dir::Up) => Dir::Right,
            (Obj::MirrorUpRight, Dir::Left) => Dir::Down,
            (Obj::MirrorUpRight, Dir::Right) => Dir::Up,
            (Obj::MirrorDownRight, Dir::Down) => Dir::Right,
            (Obj::MirrorDownRight, Dir::Up) => Dir::Left,
            (Obj::MirrorDownRight, Dir::Left) => Dir::Up,
            (Obj::MirrorDownRight, Dir::Right) => Dir::Down,
            (_, dir) => *dir,
        }
    }

    fn new_beam_from(&self, from: &Dir) -> Option<Dir> {
        match (self, from) {
            (Obj::SplitLeftRight, Dir::Down) => Some(Dir::Right),
            (Obj::SplitLeftRight, Dir::Up) => Some(Dir::Left),
            (Obj::SplitUpDown, Dir::Left) => Some(Dir::Up),
            (Obj::SplitUpDown, Dir::Right) => Some(Dir::Down),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Contraption {
    width: usize,
    height: usize,
    objects: HashMap<Pos, Obj>,
    energized: HashMap<Pos, HashSet<Dir>>,
}

impl Contraption {
    fn send_beam(&mut self, facing: Dir, from: &Pos) {
        let mut cur_pos = *from;
        let mut cur_dir = facing;

        self.energized
            .entry(cur_pos)
            .and_modify(|beams| {
                beams.insert(cur_dir);
            })
            .or_insert(HashSet::from([cur_dir]));

        loop {
            let pos = cur_dir.step_from(&cur_pos).and_then(|next_pos| {
                if next_pos.0 < self.width && next_pos.1 < self.height {
                    Some(next_pos)
                } else {
                    None
                }
            });

            match pos {
                None => break,
                Some(next_pos) => {
                    let next_obj = self.objects.get(&next_pos);
                    let next_dir: Dir = match next_obj {
                        None => cur_dir,
                        Some(obj) => obj.continue_from(&cur_dir),
                    };
                    let new_beam: Option<Dir> = match next_obj {
                        None => None,
                        Some(obj) => obj.new_beam_from(&cur_dir),
                    };

                    match self.energized.get_mut(&next_pos) {
                        Some(beams) if beams.contains(&cur_dir) => {
                            break;
                        }
                        Some(beams) => {
                            beams.insert(cur_dir);
                        }
                        None => {
                            let set = HashSet::from([cur_dir]);
                            self.energized.insert(next_pos, set);
                        }
                    }

                    if let Some(dir) = new_beam {
                        self.send_beam(dir, &next_pos);
                    }

                    cur_dir = next_dir;
                    cur_pos = next_pos;
                }
            }
        }
    }

    fn send_beam_starting(&mut self, facing: Dir, start: &Pos) {
        let dir = match self.objects.get(start) {
            Some(obj) => obj.continue_from(&facing),
            _ => facing,
        };

        self.send_beam(dir, start);
    }
}

fn main() {
    let input = include_str!("../../inputs/day_16.txt");
    let contraption = parse_contraption(&input).expect("can parse input").1;

    println!("{}", part_1(&contraption));
    println!("{}", part_2(&contraption));
}

fn part_1(contraption: &Contraption) -> usize {
    let mut cont = contraption.clone();
    cont.send_beam_starting(Dir::Right, &(0, 0));
    cont.energized.len()
}

fn part_2(contraption: &Contraption) -> usize {
    let mut max_energy = 0;

    for (dir, y) in vec![(Dir::Down, 0), (Dir::Up, contraption.height - 1)] {
        for x in 0..contraption.width {
            let mut cont = contraption.clone();
            cont.send_beam_starting(dir, &(x, y));
            max_energy = usize::max(cont.energized.len(), max_energy);
        }
    }

    for (dir, x) in vec![(Dir::Right, 0), (Dir::Left, contraption.width - 1)] {
        for y in 0..contraption.height {
            let mut cont = contraption.clone();
            cont.send_beam_starting(dir, &(x, y));
            max_energy = usize::max(cont.energized.len(), max_energy);
        }
    }

    max_energy
}

fn parse_object(input: &str) -> Parser<Obj> {
    nom::branch::alt((
        nom::combinator::value(Obj::MirrorUpRight, nom_char::char('/')),
        nom::combinator::value(Obj::MirrorDownRight, nom_char::char('\\')),
        nom::combinator::value(Obj::SplitLeftRight, nom_char::char('-')),
        nom::combinator::value(Obj::SplitUpDown, nom_char::char('|')),
    ))(input)
}

fn parse_space(input: &str) -> Parser<Option<Obj>> {
    nom::branch::alt((
        nom::combinator::map(parse_object, Some),
        nom::combinator::value(None, nom_char::char('.')),
    ))(input)
}

fn parse_row(input: &str) -> Parser<Vec<Option<Obj>>> {
    let (input, spaces) = nom::multi::many1(parse_space)(input)?;
    Ok((input, spaces))
}

fn parse_contraption(input: &str) -> Parser<Contraption> {
    let (input, rows) =
        nom::multi::separated_list1(nom_char::newline, parse_row)(input)?;
    let input = nom_char::multispace0(input)?.0;
    let input = nom::combinator::eof(input)?.0;

    let width = rows[0].len();
    let height = rows[0].len();
    let mut objects = HashMap::new();

    for (y, row) in rows.iter().enumerate() {
        for (x, val) in row.iter().enumerate() {
            if let Some(obj) = val {
                objects.insert((x, y), *obj);
            }
        }
    }

    let contraption = Contraption {
        width,
        height,
        objects,
        energized: HashMap::new(),
    };
    Ok((input, contraption))
}
