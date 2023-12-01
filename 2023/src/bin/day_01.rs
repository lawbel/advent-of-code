use std::collections::BTreeMap;

fn part_1(input: &str) -> u32 {
    input.lines().map(calibration_1).sum()
}

fn calibration_1(line: &str) -> u32 {
    let digits: Vec<u32> = get_digit_literals(line).into_values().collect();
    let tens = digits[0];
    let ones = digits[digits.len() - 1];
    (10 * tens) + ones
}

/// Returns a map of all digit literals (`0`, `1`, ... `9`) in the given
/// string, where the key is the index where it occurs and the value is the
/// numeric value of that digit.
fn get_digit_literals(line: &str) -> BTreeMap<usize, u32> {
    line.char_indices()
        .filter_map(|(i, chr)| {
            chr.to_digit(10).map(|digit| (i, digit))
        })
        .collect()
}

fn part_2(input: &str) -> u32 {
    input.lines().map(calibration_2).sum()
}

fn calibration_2(line: &str) -> u32 {
    let digit_names = get_digit_names(line);
    let digit_literals = get_digit_literals(line);

    let mut digits = digit_names;
    digits.extend(digit_literals);

    let &tens = digits.first_key_value().unwrap().1;
    let &ones = digits.last_key_value().unwrap().1;
    (10 * tens) + ones
}

/// Returns a map of all non-zero digit names (`one`, `two` ... `nine`) in the
/// given string, where the key is the index where it occurs and the value is
/// the numeric value of that digit.
fn get_digit_names(line: &str) -> BTreeMap<usize, u32> {
    let digits = BTreeMap::from([
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ]);

    let mut digit_names = BTreeMap::new();
    for (name, value) in digits {
        for (loc, _) in line.match_indices(name) {
            digit_names.insert(loc, value);
        }
    }

    digit_names
}

fn main() {
    let input = include_str!("../../inputs/day_01.txt");
    println!("{}", part_1(input));
    println!("{}", part_2(input));
}
