fn main() {
    let input = include_str!("../../inputs/day_15.txt");
    let init_seq = parse_init_seq(input);

    println!("{}", part_1(&init_seq));
}

fn part_1(init_seq: &Vec<String>) -> u32 {
    init_seq.iter().map(|seq| hash(seq) as u32).sum()
}

fn hash(string: &str) -> u8 {
    let mut value: u8 = 0;

    for byte in string.bytes() {
        value = value.wrapping_add(byte);
        value = value.wrapping_mul(17);
    }

    value
}

fn parse_init_seq(input: &str) -> Vec<String> {
    input.replace("\n", "").split(',').map(String::from).collect()
}
