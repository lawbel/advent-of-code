fn part_1(input: &str) -> u32 {
    input.lines().map(calibration).sum()
}

fn calibration(line: &str) -> u32 {
    let digits: Vec<u32> = line
        .chars()
        .filter_map(|char| char.to_digit(10))
        .collect();
    let tens = digits[0];
    let ones = digits[digits.len() - 1];
    (10 * tens) + ones
}

fn main() {
    let input = include_str!("../../inputs/day_01.txt");
    println!("{}", part_1(input));
}
