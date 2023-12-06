use nom::{bytes::complete as nom_byte, character::complete as nom_char};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Race {
    time: i64,
    record: i64,
}

fn parse_races(input: &str) -> nom::IResult<&str, Vec<Race>> {
    let input = nom_byte::tag_no_case("time:")(input)?.0;
    let input = nom_char::space1(input)?.0;
    let (input, times) =
        nom::multi::separated_list1(nom_char::space1, nom_char::i64)(input)?;
    let input = nom_char::newline(input)?.0;

    let input = nom_byte::tag_no_case("distance:")(input)?.0;
    let input = nom_char::space1(input)?.0;
    let (input, records) =
        nom::multi::separated_list1(nom_char::space1, nom_char::i64)(input)?;

    if times.len() == records.len() {
        let races = times
            .into_iter()
            .zip(records)
            .map(|(time, record)| Race { time, record })
            .collect();
        Ok((input, races))
    } else {
        // Quick and dirty fail without any error message.
        nom::combinator::fail(input)
    }
}

fn wait_needed_to_get(result: f64, race: &Race) -> Option<(f64, f64)> {
    // Note: `result = wait_for * (race.time - wait_for)`. Thus (writing
    // `half = race.time / 2` for brevity):
    //
    // `wait_for = half +/- sqrt( pow(half, 2) - result )`
    let race_time = race.time as f64;
    let half_time = race_time / 2.0;
    let discriminant = (half_time * half_time) - result;

    if discriminant >= 0.0 {
        let drift = f64::sqrt(discriminant);
        Some((half_time - drift, half_time + drift))
    } else {
        None
    }
}

fn num_waits_better_than(result: f64, race: &Race) -> i64 {
    match wait_needed_to_get(result, &race) {
        None => 0,
        Some((min, max)) => {
            let mut lower = min.ceil();
            let mut upper = max.floor();

            // We need to beat the result, not merely match it. So for example,
            // if result would be matched by at-least 10 then the least wait
            // time beating that is actually 11.
            if lower == min {
                lower += 1.0;
            }
            // Same logic as above for the upper bounds.
            if upper == max {
                upper -= 1.0;
            }

            (upper - lower + 1.0) as i64
        }
    }
}

fn part_1(races: &Vec<Race>) -> i64 {
    races
        .iter()
        .map(|race| num_waits_better_than(race.record as f64, race))
        .product()
}

fn part_2(races: &Vec<Race>) -> i64 {
    let mut time = String::new();
    let mut record = String::new();

    for race in races {
        time.push_str(&race.time.to_string());
        record.push_str(&race.record.to_string());
    }

    let big_race = Race {
        time: time.parse().unwrap(),
        record: record.parse().unwrap(),
    };

    num_waits_better_than(big_race.record as f64, &big_race)
}

fn main() {
    let input = include_str!("../../inputs/day_06.txt");
    let races = parse_races(&input).expect("can parse input").1;

    println!("{}", part_1(&races));
    println!("{}", part_2(&races));
}
