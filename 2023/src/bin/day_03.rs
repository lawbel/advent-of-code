use std::collections::HashMap;

type Grid<T> = Vec<Vec<T>>;
type Pair = (usize, usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Pos<T> {
    start: T,
    length: usize,
}

fn group_by<T>(vec: &Vec<T>, fun: impl Fn(T) -> bool) -> Vec<Vec<T>>
where
    T: Copy,
{
    let mut groups = vec![vec![]];
    let mut cur_group = 0;

    let len = vec.len();
    for i in 0..len {
        groups[cur_group].push(vec[i]);
        if (i + 1 < len) && fun(vec[i + 1]) != fun(vec[i]) {
            cur_group += 1;
            groups.push(vec![]);
        }
    }

    groups
}

fn find_numbers_in_row(line: &Vec<char>) -> HashMap<Pos<usize>, u32> {
    let mut numbers = HashMap::new();

    let mut i = 0;
    for group in group_by(line, |chr| chr.is_digit(10)) {
        let length = group.len();

        if group[0].is_digit(10) {
            let num = group.iter().collect::<String>().parse().unwrap();
            let pos = Pos { start: i, length };
            numbers.insert(pos, num);
        }

        i += length;
    }

    numbers
}

fn find_numbers_in_grid(grid: &Grid<char>) -> HashMap<Pos<Pair>, u32> {
    let mut numbers = HashMap::new();

    for (y, row) in grid.iter().enumerate() {
        for (row_pos, value) in find_numbers_in_row(row) {
            let pos = Pos {
                start: (row_pos.start, y),
                length: row_pos.length,
            };
            numbers.insert(pos, value);
        }
    }

    numbers
}

fn neighbours_of(
    grid: &Grid<char>,
    Pos { start, length }: &Pos<Pair>,
) -> Vec<char> {
    let x_start = usize::saturating_sub(start.0, 1);
    let x_end = start.0 + length;
    let y_start = usize::saturating_sub(start.1, 1);
    let y_end = start.1 + 1;
    let width = grid[0].len();
    let height = grid.len();

    let mut result = Vec::new();

    for y in y_start..=y_end {
        for x in x_start..=x_end {
            let in_bounds = (x < width) && (y < height);
            let x_in_pos = (start.0 <= x) && (x < start.0 + length);
            let y_in_pos = y == start.1;

            if in_bounds && !(x_in_pos && y_in_pos) {
                result.push(grid[y][x]);
            }
        }
    }

    result
}

fn is_symbol(chr: &char) -> bool {
    !(chr.is_digit(10) || *chr == '.')
}

fn part_numbers(
    grid: &Grid<char>,
    positions: &HashMap<Pos<Pair>, u32>,
) -> Vec<u32> {
    let mut parts = Vec::new();

    for (pos, &value) in positions {
        if neighbours_of(grid, pos).iter().any(is_symbol) {
            parts.push(value);
        }
    }

    parts
}

fn part_1(grid: &Grid<char>, positions: &HashMap<Pos<Pair>, u32>) -> u32 {
    part_numbers(grid, positions).into_iter().sum()
}

fn main() {
    let input = include_str!("../../inputs/day_03.txt");
    let grid: Grid<char> =
        input.lines().map(|line| line.chars().collect()).collect();
    let positions = find_numbers_in_grid(&grid);

    println!("{}", part_1(&grid, &positions));
}
