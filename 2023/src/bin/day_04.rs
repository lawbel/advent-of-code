use nom::{bytes::complete as nom_byte, character::complete as nom_char};
use std::collections::BTreeMap;
use std::collections::BTreeSet;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Card {
    id: u32,
    winners: BTreeSet<u32>,
    actual: BTreeSet<u32>,
}

impl Card {
    fn matches(&self) -> Vec<u32> {
        BTreeSet::intersection(&self.winners, &self.actual)
            .copied()
            .collect()
    }

    fn num_matches(&self) -> u32 {
        self.matches()
            .len()
            .try_into()
            .expect("number of matches fits in a u32")
    }

    fn score(&self) -> u32 {
        match self.num_matches() {
            0 => 0,
            n => u32::pow(2, n - 1),
        }
    }
}

fn parse_cards(input: &str) -> nom::IResult<&str, BTreeMap<u32, Card>> {
    let (input, cards) =
        nom::multi::separated_list1(nom_char::multispace0, parse_card)(input)?;
    let cards =
        BTreeMap::from_iter(cards.into_iter().map(|card| (card.id, card)));
    Ok((input, cards))
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

fn part_1(cards: &BTreeMap<u32, Card>) -> u32 {
    cards.values().map(Card::score).sum()
}

fn play_cards(cards: &BTreeMap<u32, Card>) -> BTreeMap<u32, u32> {
    let mut copies = BTreeMap::from_iter(
        cards.keys().cloned().map(|id| (id, 1)).collect::<Vec<_>>(),
    );

    // We traverse in order from smallest to biggest `id`, which is specified
    // behaviour when iterating a [`BTreeMap`].
    for (id, card) in cards {
        let &factor = copies.get(id).unwrap_or(&1);
        for i in 1..=card.num_matches() {
            copies.entry(id + i).and_modify(|num| *num += factor);
        }
    }

    copies
}

fn part_2(cards: &BTreeMap<u32, Card>) -> u32 {
    play_cards(cards).values().sum()
}

fn main() {
    let input = include_str!("../../inputs/day_04.txt");
    let cards = parse_cards(input).expect("able to parse cards").1;

    println!("{}", part_1(&cards));
    println!("{}", part_2(&cards));
}
