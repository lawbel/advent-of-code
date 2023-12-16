use nom::character::complete as nom_char;
use std::fmt;

type Parser<'a, T> = nom::IResult<&'a str, T>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Step<'a> {
    label: &'a str,
    op: Operation,
}

impl<'a> fmt::Display for Step<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.label, self.op)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Operation {
    Remove,
    Insert(u8),
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Remove => write!(f, "-"),
            Self::Insert(n) => write!(f, "={}", n),
        }
    }
}

type Boxes<'a> = [Box<'a>; 256];

type Box<'a> = Vec<Lens<'a>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Lens<'a> {
    label: &'a str,
    focal_len: u8,
}

impl<'a> fmt::Display for Lens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} {}]", self.label, self.focal_len)
    }
}

fn main() {
    let input = include_str!("../../inputs/day_15.txt");
    let init_seq = parse_init_seq(input).expect("can parse input").1;

    println!("{}", part_1(&init_seq));
    println!("{}", part_2(&init_seq));
}

fn part_1(init_seq: &Vec<Step>) -> u32 {
    init_seq
        .iter()
        .map(|step| hash(&format!("{step}")) as u32)
        .sum()
}

fn part_2(init_seq: &Vec<Step>) -> usize {
    let mut boxes: Boxes = array_init::array_init(|_| Vec::new());
    for step in init_seq {
        apply_step(&step, &mut boxes);
    }

    let mut focusing_power = 0;
    for (box_num, r#box) in boxes.iter().enumerate() {
        for (lens_num, lens) in r#box.iter().enumerate() {
            focusing_power +=
                (1 + box_num) * (1 + lens_num) * (lens.focal_len as usize);
        }
    }

    focusing_power
}

fn apply_step<'a>(step: &Step<'a>, boxes: &mut Boxes<'a>) {
    let index = hash(step.label) as usize;
    let r#box = &mut boxes[index];
    let label_pos = r#box.iter().position(|lens| lens.label == step.label);

    match step.op {
        Operation::Remove => {
            if let Some(i) = label_pos {
                r#box.remove(i);
            }
        }
        Operation::Insert(focal_len) => match label_pos {
            Some(i) => {
                r#box[i].focal_len = focal_len;
            }
            None => {
                r#box.push(Lens {
                    label: step.label,
                    focal_len,
                });
            }
        },
    }
}

fn hash(string: &str) -> u8 {
    let mut value: u8 = 0;

    for byte in string.bytes() {
        value = value.wrapping_add(byte);
        value = value.wrapping_mul(17);
    }

    value
}

fn parse_init_seq(input: &str) -> Parser<Vec<Step>> {
    nom::multi::separated_list1(nom_char::char(','), parse_step)(input)
}

fn parse_step(input: &str) -> Parser<Step> {
    let (input, label) = nom_char::alpha1(input)?;
    let (input, op) = parse_operation(input)?;
    Ok((input, Step { label, op }))
}

fn parse_operation(input: &str) -> Parser<Operation> {
    nom::branch::alt((
        nom::combinator::value(Operation::Remove, nom_char::char('-')),
        nom::combinator::map(
            nom::sequence::preceded(nom_char::char('='), nom_char::u8),
            Operation::Insert,
        ),
    ))(input)
}
