use nom::character::complete as nom_char;
use std::collections::HashMap;
use std::hash::Hash;

type Network<T> = HashMap<T, (T, T)>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Choice {
    R,
    L,
}

/// Parses a choice - either `L` or `R`.
fn parse_choice(input: &str) -> nom::IResult<&str, Choice> {
    nom::branch::alt((
        nom::combinator::value(Choice::R, nom_char::char('R')),
        nom::combinator::value(Choice::L, nom_char::char('L')),
    ))(input)
}

/// Parses a node like `AAA = (BBB, CCC)`.
fn parse_node(input: &str) -> nom::IResult<&str, (&str, (&str, &str))> {
    let (input, node) = nom_char::alpha1(input)?;
    let input = nom_char::space0(input)?.0;
    let input = nom_char::char('=')(input)?.0;
    let input = nom_char::space0(input)?.0;
    let input = nom_char::char('(')(input)?.0;
    let (input, left) = nom_char::alpha1(input)?;
    let input = nom_char::char(',')(input)?.0;
    let input = nom_char::space0(input)?.0;
    let (input, right) = nom_char::alpha1(input)?;
    let input = nom_char::char(')')(input)?.0;
    Ok((input, (node, (left, right))))
}

/// Parses choices and network, like:
///
///     RRRLLRLRL
///
///     AAA = (BBB, CCC)
///     BBB = (DDD, EEE)
fn parse_choices_network(
    input: &str,
) -> nom::IResult<&str, (Vec<Choice>, Network<&str>)> {
    let (input, choices) = nom::multi::many1(parse_choice)(input)?;
    let input = nom_char::multispace0(input)?.0;
    let (input, nodes) =
        nom::multi::separated_list1(nom_char::multispace0, parse_node)(input)?;

    let network = HashMap::from_iter(nodes.into_iter());
    Ok((input, (choices, network)))
}

/// In the given [`Network`], cycle through the given choices starting from
/// `start` until we reach `stop`.
fn walk_from_to<T>(
    start: T,
    stop: T,
    choices: &Vec<Choice>,
    network: &Network<T>,
) -> Option<Vec<T>>
where
    T: Eq + Hash + Copy,
{
    let len = choices.len();
    if len == 0 {
        return None;
    }

    let mut path = Vec::new();
    let mut current = start;
    let mut choice = 0;

    loop {
        path.push(current);

        if current == stop {
            return Some(path);
        }

        match network.get(&current) {
            None => {
                return None;
            }
            Some((left, right)) => {
                current = match choices[choice] {
                    Choice::L => *left,
                    Choice::R => *right,
                };
            }
        }

        choice = (choice + 1) % len;
    }
}

fn part_1(choices: &Vec<Choice>, network: &Network<&str>) -> Option<usize> {
    let start = String::from("AAA");
    let stop = String::from("ZZZ");
    let path = walk_from_to(&start[..], &stop[..], choices, network)?;
    Some(path.len() - 1)
}

fn main() {
    let input = include_str!("../../inputs/day_08.txt");
    let (choices, network) =
        parse_choices_network(&input).expect("can parse input").1;

    println!("{}", part_1(&choices, &network).expect("can solve part 1"));
}
