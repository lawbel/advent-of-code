use itertools::Itertools as iter;
use nom::character::complete as nom_char;
use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Card {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Hand {
    cards: [Card; 5],
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

impl Hand {
    fn hand_type(&self) -> HandType {
        let groups = iter::counts(self.cards.iter());
        let counts: Vec<_> = groups.values().sorted().collect();
        match counts[..] {
            [5] => HandType::FiveOfAKind,
            [1, 4] => HandType::FourOfAKind,
            [2, 3] => HandType::FullHouse,
            [1, 1, 3] => HandType::ThreeOfAKind,
            [1, 2, 2] => HandType::TwoPair,
            [1, 1, 1, 2] => HandType::OnePair,
            [1, 1, 1, 1, 1] => HandType::HighCard,
            _ => panic!("unreachable"),
        }
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.hand_type().cmp(&other.hand_type()) {
            Ordering::Equal => self.cards.cmp(&other.cards),
            not_eq => not_eq,
        }
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn parse_card(input: &str) -> nom::IResult<&str, Card> {
    nom::branch::alt((
        nom::combinator::value(Card::Two, nom_char::char('2')),
        nom::combinator::value(Card::Three, nom_char::char('3')),
        nom::combinator::value(Card::Four, nom_char::char('4')),
        nom::combinator::value(Card::Five, nom_char::char('5')),
        nom::combinator::value(Card::Six, nom_char::char('6')),
        nom::combinator::value(Card::Seven, nom_char::char('7')),
        nom::combinator::value(Card::Eight, nom_char::char('8')),
        nom::combinator::value(Card::Nine, nom_char::char('9')),
        nom::combinator::value(Card::Ten, nom_char::char('T')),
        nom::combinator::value(Card::Jack, nom_char::char('J')),
        nom::combinator::value(Card::Queen, nom_char::char('Q')),
        nom::combinator::value(Card::King, nom_char::char('K')),
        nom::combinator::value(Card::Ace, nom_char::char('A')),
    ))(input)
}

fn parse_hand(input: &str) -> nom::IResult<&str, Hand> {
    let (input, cards) = nom::multi::count(parse_card, 5)(input)?;
    let hand = Hand {
        cards: cards.try_into().unwrap(),
    };
    Ok((input, hand))
}

fn parse_hand_bid(input: &str) -> nom::IResult<&str, (Hand, i32)> {
    let (input, hand) = parse_hand(input)?;
    let input = nom_char::space1(input)?.0;
    let (input, bid) = nom_char::i32(input)?;
    Ok((input, (hand, bid)))
}

fn parse_hands_bids(input: &str) -> nom::IResult<&str, Vec<(Hand, i32)>> {
    nom::multi::separated_list1(nom_char::multispace0, parse_hand_bid)(input)
}

fn part_1(hands_bids: &Vec<(Hand, i32)>) -> i32 {
    hands_bids
        .iter()
        .sorted()
        .enumerate()
        .map(|(rank, (_, bid))| bid * (rank as i32 + 1))
        .sum()
}

fn main() {
    let input = include_str!("../../inputs/day_07.txt");
    let hands_bids = parse_hands_bids(&input).expect("can parse input").1;

    println!("{}", part_1(&hands_bids));
}
