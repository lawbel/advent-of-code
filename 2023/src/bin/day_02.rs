use nom;
use nom::{bytes::complete as nom_byte, character::complete as nom_char};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Game {
    id: u32,
    observations: Vec<Observation>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Observation {
    green: Option<u32>,
    blue: Option<u32>,
    red: Option<u32>,
}

impl Game {
    fn is_possible(&self) -> bool {
        for obs in &self.observations {
            let reds = obs.red.unwrap_or(0);
            let greens = obs.green.unwrap_or(0);
            let blues = obs.blue.unwrap_or(0);

            if (reds > 12) || (greens > 13) || (blues > 14) {
                return false;
            }
        }
        return true;
    }

    fn minimal(&self) -> Observation {
        let mut result = Observation {
            green: None,
            red: None,
            blue: None,
        };

        for obs in &self.observations {
            obs.red.map(|n| {
                result.red = Some(match result.red {
                    None => n,
                    Some(m) => u32::max(n, m),
                })
            });
            obs.blue.map(|n|
                result.blue = Some(match result.blue {
                    None => n,
                    Some(m) => u32::max(n, m),
                })
            );
            obs.green.map(|n| {
                result.green = Some(match result.green {
                    None => n,
                    Some(m) => u32::max(n, m),
                })
            });
        }

        result
    }
}

impl Observation {
    fn power(&self) -> u32 {
        self.green.unwrap_or(0)
            * self.blue.unwrap_or(0)
            * self.red.unwrap_or(0)
    }
}

/// Parses a sequence of games, separated by newlines (or other whitespace).
/// A game is parsed as described in [`parse_game`].
fn parse_games(input: &str) -> nom::IResult<&str, Vec<Game>> {
    let (input, games) =
        nom::multi::separated_list1(nom_char::multispace0, parse_game)(input)?;
    let input = nom_char::multispace0(input)?.0;
    Ok((input, games))
}

/// Parses a string like `game {num}: {obs1} {obs2} {obs3}` where `num` is
/// simply a [`u32`] and `obs1`, `obs2`, `obs3` are strings formatted as
/// described in [`parse_observation`]. Returns a [`Game`] which contains the
/// `num` and observations as fields.
///
/// For example:
///
/// * `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green`
/// * `Game 2: 1 blue, 22 green`
fn parse_game(input: &str) -> nom::IResult<&str, Game> {
    let input = nom_byte::tag_no_case("game")(input)?.0;
    let input = nom_char::space1(input)?.0;
    let (input, id) = nom_char::u32(input)?;
    let input = nom_char::char(':')(input)?.0;
    let input = nom_char::space1(input)?.0;
    let (input, observations) = nom::multi::many1(parse_observation)(input)?;

    let game = Game { id, observations };
    Ok((input, game))
}

/// Parses a string like `{n1} {c1}, {n2} {c2}, {n3} {c3}; ` where
///
/// * `c1`, `c2`, `c3` should be distinct values from {green, blue, red};
/// * `n1`, `n2`, `n3` are simply [`u32`]s.
///
/// Then returns an [`Observation`] with tbe colour fields populated by the
/// associated [`u32`]s.
///
/// The full string is not mandatory, it can be just `{n1} {c1}, {n2} {c2}` or
/// `{n1} {c1}` and the final semicolon is optional. Note that each `{n} {c}`
/// pair is the same as that from [`parse_cube`].
///
/// For example:
///
/// * `2 green`
/// * `3 green, 4 blue, 1 red`
/// * `3 blue, 4 red;`
fn parse_observation(input: &str) -> nom::IResult<&str, Observation> {
    let (input, cubes) = nom::multi::many1(parse_cube)(input)?;
    let input = nom::combinator::opt(nom_char::char(';'))(input)?.0;
    let input = nom_char::space0(input)?.0;

    let mut green = None;
    let mut blue = None;
    let mut red = None;

    for (num, colour) in cubes {
        match colour {
            "red" => { red = Some(red.unwrap_or(0) + num); }
            "blue" => { blue = Some(blue.unwrap_or(0) + num); }
            "green" => { green = Some(green.unwrap_or(0) + num); }
            _ => {}
        }
    }

    let obs = Observation { green, blue, red };
    Ok((input, obs))
}

/// Parses a string like `{number} {colour}, ` where `colour` is one of
/// {red, green, blue} and the comma is optional. Returns the `number` as a
/// [`u32`]. For example:
///
/// * `3 blue, `
/// * `20 red`
/// * `2 green, `
fn parse_cube(input: &str) -> nom::IResult<&str, (u32, &str)> {
    let (input, count) = nom_char::u32(input)?;
    let input = nom_char::space1(input)?.0;
    let (input, colour) = parse_colour(input)?;
    let input = nom::combinator::opt(nom_char::char(','))(input)?.0;
    let input = nom_char::space0(input)?.0;

    let result = (count, colour);
    Ok((input, result))
}

fn parse_colour(input: &str) -> nom::IResult<&str, &str> {
    nom::branch::alt((
        nom_byte::tag_no_case("green"),
        nom_byte::tag_no_case("blue"),
        nom_byte::tag_no_case("red"),
    ))(input)
}

fn part_1(games: &Vec<Game>) -> u32 {
    games
        .into_iter()
        .filter(|game| game.is_possible())
        .map(|game| game.id)
        .sum()
}

fn part_2(games: &Vec<Game>) -> u32 {
    games.into_iter().map(|game| game.minimal().power()).sum()
}

fn main() {
    let input = include_str!("../../inputs/day_02.txt");
    let games = parse_games(input).expect("couldn't parse input games").1;
    println!("{}", part_1(&games));
    println!("{}", part_2(&games));
}
