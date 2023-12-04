use nom::{bytes::complete as nom_byte, character::complete as nom_char};
use std::collections::BTreeSet;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Card {
    id: u32,
    winners: BTreeSet<u32>,
    actual: BTreeSet<u32>,
}

impl Card {
    fn score(&self) -> u32 {
        let matches = BTreeSet::intersection(&self.winners, &self.actual)
            .copied()
            .collect::<Vec<_>>()
            .len();

        let matches =
            matches.try_into().expect("size of matches fits in a u32");
        match matches {
            0 => 0,
            _ => u32::pow(2, matches - 1),
        }
    }
}

fn parse_cards(input: &str) -> nom::IResult<&str, Vec<Card>> {
    nom::multi::separated_list1(nom_char::multispace0, parse_card)(input)
}

fn parse_card(input: &str) -> nom::IResult<&str, Card> {
    let input = nom_byte::tag_no_case("card")(input)?.0;
    let input = nom_char::space1(input)?.0;
    let (input, id) = nom_char::u32(input)?;
    let input = nom_char::char(':')(input)?.0;
    let input = nom_char::space1(input)?.0;
    let (input, winners) =
        nom::multi::separated_list1(nom_char::space1, nom_char::u32)(input)?;
    let input = nom_char::space1(input)?.0;
    let input = nom_char::char('|')(input)?.0;
    let input = nom_char::space1(input)?.0;
    let (input, actual) =
        nom::multi::separated_list1(nom_char::space1, nom_char::u32)(input)?;

    let card = Card {
        id,
        winners: BTreeSet::from_iter(winners.into_iter()),
        actual: BTreeSet::from_iter(actual.into_iter()),
    };
    Ok((input, card))
}

fn part_1(cards: &Vec<Card>) -> u32 {
    cards.iter().map(Card::score).sum()
}

fn main() {
    let input = include_str!("../../inputs/day_04.txt");
    let cards = parse_cards(input).expect("unable to parse cards").1;

    println!("{}", part_1(&cards));
}
