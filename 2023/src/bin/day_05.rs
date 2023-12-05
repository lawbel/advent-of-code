use gcollections::ops::*;
use interval::interval_set::ToIntervalSet;
use interval::ops::*;
use interval::{Interval, IntervalSet};
use nom::{bytes::complete as nom_byte, character::complete as nom_char};

#[derive(Debug, PartialEq, Eq)]
struct IntervalMap {
    dest: Interval<i64>,
    source: Interval<i64>,
    shift: i64,
}

#[derive(Debug, PartialEq, Eq)]
struct Mapping<'a> {
    maps: Vec<IntervalMap>,
    from: &'a str,
    to: &'a str,
}

fn parse_seeds(input: &str) -> nom::IResult<&str, Vec<i64>> {
    let input = nom_byte::tag_no_case("seeds")(input)?.0;
    let input = nom_char::char(':')(input)?.0;
    let input = nom_char::space1(input)?.0;
    nom::multi::separated_list1(nom_char::space1, nom_char::i64)(input)
}

fn parse_map(input: &str) -> nom::IResult<&str, IntervalMap> {
    let (input, dest_start) = nom_char::i64(input)?;
    let input = nom_char::space1(input)?.0;
    let (input, src_start) = nom_char::i64(input)?;
    let input = nom_char::space1(input)?.0;
    let (input, len) = nom_char::i64(input)?;

    let map = IntervalMap {
        dest: Interval::new(dest_start, dest_start + len),
        source: Interval::new(src_start, src_start + len),
        shift: dest_start - src_start,
    };
    Ok((input, map))
}

fn parse_maps(input: &str) -> nom::IResult<&str, Vec<IntervalMap>> {
    nom::multi::separated_list1(nom_char::multispace1, parse_map)(input)
}

fn parse_mapping(input: &str) -> nom::IResult<&str, Mapping> {
    let (input, from) = nom_char::alpha1(input)?;
    let input = nom_char::char('-')(input)?.0;
    let input = nom_byte::tag_no_case("to")(input)?.0;
    let input = nom_char::char('-')(input)?.0;
    let (input, to) = nom_char::alpha1(input)?;
    let input = nom_char::space1(input)?.0;
    let input = nom_byte::tag_no_case("map")(input)?.0;
    let input = nom_char::char(':')(input)?.0;
    let input = nom_char::multispace1(input)?.0;
    let (input, maps) = parse_maps(input)?;

    let mapping = Mapping { maps, from, to };
    Ok((input, mapping))
}

fn parse_mappings(input: &str) -> nom::IResult<&str, Vec<Mapping>> {
    nom::multi::separated_list1(nom_char::multispace1, parse_mapping)(input)
}

fn parse_input(input: &str) -> nom::IResult<&str, (Vec<i64>, Vec<Mapping>)> {
    let (input, seeds) = parse_seeds(input)?;
    let input = nom_char::multispace1(input)?.0;
    let (input, maps) = parse_mappings(input)?;

    Ok((input, (seeds, maps)))
}

fn run_map_at(seed: i64, mapping: &Mapping) -> i64 {
    for map in &mapping.maps {
        if (map.source.lower() <= seed) && (seed < map.source.upper()) {
            return seed + map.shift;
        }
    }
    return seed;
}

fn run_maps_at(seed: i64, mappings: &Vec<Mapping>) -> i64 {
    let mut value = seed;
    for map in mappings {
        value = run_map_at(value, map);
    }
    value
}

fn part_1(seeds: &Vec<i64>, mappings: &Vec<Mapping>) -> i64 {
    seeds
        .iter()
        .map(|seed| run_maps_at(*seed, mappings))
        .min()
        .expect("there is a minimal seed")
}

fn run_map_over(
    interval: &Interval<i64>,
    mapping: &Mapping,
) -> IntervalSet<i64> {
    let mut image = IntervalSet::empty();

    // Add the image of the parts which _do_ overlap with source intervals.
    for map in &mapping.maps {
        let overlap = interval.intersection(&map.source);
        if !overlap.is_empty() {
            let shifted = overlap + map.shift;
            image = image.union(&interval_into_set(&shifted));
        }
    }

    // Add the image of the parts which _don't_ overlap with source intervals.
    let source_sets = interval_maps_to_set(&mapping.maps);
    let set = interval_into_set(interval);
    image = image.union(&set.difference(&source_sets));

    image
}

fn run_maps_over(
    interval: &Interval<i64>,
    mappings: &Vec<Mapping>,
) -> IntervalSet<i64> {
    let mut image = interval_into_set(interval);

    for mapping in mappings {
        image = image
            .iter()
            .map(|int| run_map_over(int, mapping))
            .fold(IntervalSet::empty(), |a, b| a.union(&b));
    }

    image
}

fn interval_maps_to_set(maps: &Vec<IntervalMap>) -> IntervalSet<i64> {
    let mut pairs = maps
        .iter()
        .map(|map| (map.source.lower(), map.source.upper()))
        .collect::<Vec<_>>();

    // This sort is necessary to avoid a panic - a bug in the library?
    pairs.sort();
    pairs.to_interval_set()
}

fn interval_into_set(interval: &Interval<i64>) -> IntervalSet<i64> {
    (interval.lower(), interval.upper()).to_interval_set()
}

fn part_2(seeds: &Vec<i64>, mappings: &Vec<Mapping>) -> i64 {
    let mut results = Vec::new();

    for i in (0..seeds.len()).step_by(2) {
        let start = seeds[i];
        let len = seeds[i + 1];
        let interval = Interval::new(start, start + len);
        results.push(run_maps_over(&interval, mappings));
    }

    results
        .iter()
        .map(IntervalSet::lower)
        .min()
        .expect("non-zero amount of seeds")
}

fn main() {
    let input = include_str!("../../inputs/day_05.txt");
    let (seeds, maps) = parse_input(input).expect("can parse input").1;

    println!("{}", part_1(&seeds, &maps));
    println!("{}", part_2(&seeds, &maps));
}
