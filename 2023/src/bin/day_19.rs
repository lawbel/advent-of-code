use nom::{bytes::complete as nom_byte, character::complete as nom_char};
use std::collections::HashMap;

type Parser<'a, T> = nom::IResult<&'a str, T>;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Category {
    X,
    M,
    A,
    S,
}

impl Category {
    fn parse(input: &str) -> Parser<Self> {
        nom::branch::alt((
            nom::combinator::value(Self::X, nom_char::char('x')),
            nom::combinator::value(Self::M, nom_char::char('m')),
            nom::combinator::value(Self::A, nom_char::char('a')),
            nom::combinator::value(Self::S, nom_char::char('s')),
        ))(input)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Condition<T> {
    LessThan(T),
    GreaterThan(T),
}

impl<T> Condition<T> {
    fn parse<'a>(
        input: &'a str,
        inner: impl Fn(&'a str) -> Parser<T>,
    ) -> Parser<Self> {
        nom::branch::alt((
            nom::sequence::preceded(
                nom_char::char('<'),
                nom::combinator::map(&inner, Self::LessThan),
            ),
            nom::sequence::preceded(
                nom_char::char('>'),
                nom::combinator::map(&inner, Self::GreaterThan),
            ),
        ))(input)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Dest<'a> {
    Name(&'a str),
    Accept,
    Reject,
}

impl<'a> Dest<'a> {
    fn parse(input: &'a str) -> Parser<Self> {
        nom::branch::alt((
            nom::combinator::value(Self::Accept, nom_char::char('A')),
            nom::combinator::value(Self::Reject, nom_char::char('R')),
            nom::combinator::map(nom_char::alpha1, Self::Name),
        ))(input)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct LongRule<'a> {
    cat: Category,
    cond: Condition<i32>,
    dest: Dest<'a>,
}

impl<'a> LongRule<'a> {
    fn parse(input: &'a str) -> Parser<Self> {
        let (input, cat) = Category::parse(input)?;
        let (input, cond) = Condition::parse(input, nom_char::i32)?;
        let input = nom_char::char(':')(input)?.0;
        let (input, dest) = Dest::parse(input)?;
        Ok((input, Self { cat, cond, dest }))
    }

    fn test(&self, rating: &Rating) -> Option<Dest> {
        let l = rating.category(self.cat);
        match self.cond {
            Condition::GreaterThan(r) if l > r => Some(self.dest.clone()),
            Condition::LessThan(r) if l < r => Some(self.dest.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ShortRule<'a> {
    dest: Dest<'a>,
}

impl<'a> ShortRule<'a> {
    fn parse(input: &'a str) -> Parser<Self> {
        nom::combinator::map(Dest::parse, |dest| Self { dest })(input)
    }

    fn test(&self) -> Dest {
        self.dest.clone()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Rule<'a> {
    Long(LongRule<'a>),
    Short(ShortRule<'a>),
}

impl<'a> Rule<'a> {
    fn parse(input: &'a str) -> Parser<Self> {
        nom::branch::alt((
            nom::combinator::map(LongRule::parse, Self::Long),
            nom::combinator::map(ShortRule::parse, Self::Short),
        ))(input)
    }

    fn test(&self, rating: &Rating) -> Option<Dest> {
        match self {
            Self::Long(long) => long.test(rating),
            Self::Short(short) => Some(short.test()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Workflow<'a> {
    name: &'a str,
    rules: Vec<Rule<'a>>,
}

impl<'a> Workflow<'a> {
    fn parse(input: &'a str) -> Parser<Self> {
        let (input, name) = nom_char::alpha1(input)?;
        let input = nom_char::char('{')(input)?.0;
        let (input, rules) = nom::multi::separated_list1(
            nom_char::char(','),
            Rule::parse,
        )(input)?;
        let input = nom_char::char('}')(input)?.0;
        Ok((input, Self { name, rules }))
    }

    fn flows_to(&self, rating: &Rating) -> Option<Dest> {
        for rule in self.rules.iter() {
            let dest = rule.test(rating);
            if dest.is_some() {
                return dest;
            }
        }
        None
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Rating {
    x: i32,
    m: i32,
    a: i32,
    s: i32,
}

impl Rating {
    fn parse(input: &str) -> Parser<Self> {
        let input = nom_byte::tag("{x=")(input)?.0;
        let (input, x) = nom_char::i32(input)?;
        let input = nom_byte::tag(",m=")(input)?.0;
        let (input, m) = nom_char::i32(input)?;
        let input = nom_byte::tag(",a=")(input)?.0;
        let (input, a) = nom_char::i32(input)?;
        let input = nom_byte::tag(",s=")(input)?.0;
        let (input, s) = nom_char::i32(input)?;
        let input = nom_char::char('}')(input)?.0;
        Ok((input, Self { x, m, a, s }))
    }

    fn category(&self, cat: Category) -> i32 {
        match cat {
            Category::X => self.x,
            Category::M => self.m,
            Category::A => self.a,
            Category::S => self.s,
        }
    }
}

fn parse_workflows_ratings(
    input: &str,
) -> Parser<(HashMap<&str, Workflow>, Vec<Rating>)> {
    let (input, workflows) = nom::multi::separated_list1(
        nom_char::newline,
        Workflow::parse,
    )(input)?;
    let input = nom_char::multispace0(input)?.0;
    let (input, ratings) =
        nom::multi::separated_list1(nom_char::newline, Rating::parse)(input)?;
    let input = nom_char::multispace0(input)?.0;
    let input = nom::combinator::eof(input)?.0;
    let workflows =
        HashMap::from_iter(workflows.into_iter().map(|w| (w.name, w)));
    Ok((input, (workflows, ratings)))
}

fn main() {
    let input = include_str!("../../inputs/day_19.txt");
    let (workflows, ratings) =
        parse_workflows_ratings(input).expect("can parse input").1;

    println!("{:#?}", part_1(&workflows, &ratings));
}

fn part_1(workflows: &HashMap<&str, Workflow>, ratings: &Vec<Rating>) -> i32 {
    let mut accepted = Vec::new();

    for rating in ratings {
        let mut next = workflows.get("in");
        // We could simply panic if a workflow points to some other
        // non-existent workflow; for now, we treat such a case by breaking
        // the loop early, effectively rejecting it.
        while let Some(dest) = next.and_then(|w| w.flows_to(rating)) {
            match dest {
                Dest::Name(name) => {
                    next = workflows.get(name);
                }
                Dest::Accept => {
                    accepted.push(rating);
                    break;
                }
                Dest::Reject => break,
            }
        }
    }

    accepted
        .iter()
        .map(|rating| rating.x + rating.m + rating.a + rating.s)
        .sum()
}
