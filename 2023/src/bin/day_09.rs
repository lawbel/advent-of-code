use nom::character::complete as nom_char;

fn parse_list(input: &str) -> nom::IResult<&str, Vec<i32>> {
    nom::multi::separated_list1(nom_char::space1, nom_char::i32)(input)
}

fn parse_lists(input: &str) -> nom::IResult<&str, Vec<Vec<i32>>> {
    let (input, lists) =
        nom::multi::separated_list1(nom_char::newline, parse_list)(input)?;
    let input = nom_char::multispace0(input)?.0;
    Ok((input, lists))
}

fn to_deltas(vals: &Vec<i32>) -> Vec<i32> {
    vals.windows(2).map(|w| w[1] - w[0]).collect()
}

fn predict_next(vals: &Vec<i32>) -> i32 {
    let mut deltas = vals.clone();
    let mut result = 0;

    while !deltas.iter().all(|&x| x == 0) {
        result += deltas[deltas.len() - 1];
        deltas = to_deltas(&deltas);
    }

    result
}

fn part_1(lists: &Vec<Vec<i32>>) -> i32 {
    lists.iter().map(predict_next).sum()
}

fn main() {
    let input = include_str!("../../inputs/day_09.txt");
    let lists = parse_lists(&input).expect("can parse input").1;

    println!("{}", part_1(&lists));
}
