use nom::{bytes::complete as nom_byte, character::complete as nom_char};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Range {
    dest_start: i64,
    source_start: i64,
    len: i64,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Mapping<'a> {
    ranges: Vec<Range>,
    from: &'a str,
    to: &'a str,
}

fn parse_seeds(input: &str) -> nom::IResult<&str, Vec<i64>> {
    let input = nom_byte::tag_no_case("seeds")(input)?.0;
    let input = nom_char::char(':')(input)?.0;
    let input = nom_char::space1(input)?.0;
    nom::multi::separated_list1(nom_char::space1, nom_char::i64)(input)
}

fn parse_range(input: &str) -> nom::IResult<&str, Range> {
    let (input, dest_start) = nom_char::i64(input)?;
    let input = nom_char::space1(input)?.0;
    let (input, source_start) = nom_char::i64(input)?;
    let input = nom_char::space1(input)?.0;
    let (input, len) = nom_char::i64(input)?;

    let range = Range {
        dest_start,
        source_start,
        len,
    };
    Ok((input, range))
}

fn parse_ranges(input: &str) -> nom::IResult<&str, Vec<Range>> {
    nom::multi::separated_list1(nom_char::multispace1, parse_range)(input)
}

fn parse_map(input: &str) -> nom::IResult<&str, Mapping> {
    let (input, from) = nom_char::alpha1(input)?;
    let input = nom_char::char('-')(input)?.0;
    let input = nom_byte::tag_no_case("to")(input)?.0;
    let input = nom_char::char('-')(input)?.0;
    let (input, to) = nom_char::alpha1(input)?;
    let input = nom_char::space1(input)?.0;
    let input = nom_byte::tag_no_case("map")(input)?.0;
    let input = nom_char::char(':')(input)?.0;
    let input = nom_char::multispace1(input)?.0;
    let (input, ranges) = parse_ranges(input)?;

    let mapping = Mapping { ranges, from, to };
    Ok((input, mapping))
}

fn parse_maps(input: &str) -> nom::IResult<&str, Vec<Mapping>> {
    nom::multi::separated_list1(nom_char::multispace1, parse_map)(input)
}

fn parse_input(input: &str) -> nom::IResult<&str, (Vec<i64>, Vec<Mapping>)> {
    let (input, seeds) = parse_seeds(input)?;
    let input = nom_char::multispace1(input)?.0;
    let (input, maps) = parse_maps(input)?;

    Ok((input, (seeds, maps)))
}

fn run_map(seed: i64, map: &Mapping) -> i64 {
    for range in &map.ranges {
        let start = range.source_start;
        let end = range.source_start + range.len;
        if (start <= seed) && (seed < end) {
            let shift = range.dest_start - range.source_start;
            return seed + shift;
        }
    }
    return seed;
}

fn run_maps(seed: i64, maps: &Vec<Mapping>) -> i64 {
    let mut value = seed;
    for map in maps {
        value = run_map(value, map);
    }
    value
}

fn part_1(seeds: &Vec<i64>, maps: &Vec<Mapping>) -> i64 {
    seeds
        .iter()
        .map(|seed| run_maps(*seed, maps))
        .min()
        .expect("there is a minimal seed")
}

fn main() {
    let input = include_str!("../../inputs/day_05.txt");
    let (seeds, maps) = parse_input(input).expect("can parse input").1;

    println!("{}", part_1(&seeds, &maps));
}
