use std::collections::HashMap;
use std::hash::Hash;

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
) -> HashMap<Pair, char> {
    let x_start = usize::saturating_sub(start.0, 1);
    let x_end = start.0 + length;
    let y_start = usize::saturating_sub(start.1, 1);
    let y_end = start.1 + 1;
    let width = grid[0].len();
    let height = grid.len();

    let mut result = HashMap::new();

    for y in y_start..=y_end {
        for x in x_start..=x_end {
            let in_bounds = (x < width) && (y < height);
            let x_in_pos = (start.0 <= x) && (x < start.0 + length);
            let y_in_pos = y == start.1;

            if in_bounds && !(x_in_pos && y_in_pos) {
                result.insert((x, y), grid[y][x]);
            }
        }
    }

    result
}

fn is_symbol(chr: &char) -> bool {
    !(chr.is_digit(10) || *chr == '.')
}

fn is_gear(chr: &char) -> bool {
    *chr == '*'
}

fn part_numbers(
    grid: &Grid<char>,
    positions: &HashMap<Pos<Pair>, u32>,
) -> Vec<u32> {
    let mut parts = Vec::new();

    for (pos, &value) in positions {
        if neighbours_of(grid, pos).values().any(is_symbol) {
            parts.push(value);
        }
    }

    parts
}

/// Intersects the two [`HashMap`]s with each other, returning the result as
/// a new map. If a key is common to both maps, then we simply use the value
/// from the first map.
fn intersect<'a, K, V>(
    hash_1: &HashMap<K, V>,
    hash_2: &HashMap<K, V>,
) -> HashMap<K, V>
where
    K: Eq + Hash + Copy,
    V: Copy,
{
    let mut new = HashMap::new();

    for (&key, &value) in hash_1.iter() {
        if hash_2.contains_key(&key) {
            new.insert(key, value);
        }
    }

    new
}

fn gear_ratios(
    grid: &Grid<char>,
    positions: &HashMap<Pos<Pair>, u32>,
) -> HashMap<Pair, u32> {
    let mut gears = HashMap::new();

    let pairs: Vec<_> = positions.keys().collect();
    let len = pairs.len();

    for i_1 in 0..len {
        for i_2 in 0..i_1 {
            let pos_1 = pairs[i_1];
            let pos_2 = pairs[i_2];
            let end_1 = pos_1.start.0 + pos_1.length;
            let end_2 = pos_2.start.0 + pos_2.length;

            let y_diff = usize::abs_diff(pos_1.start.1, pos_2.start.1);
            let x_diff_left = usize::saturating_sub(pos_2.start.0, end_1);
            let x_diff_right = usize::saturating_sub(pos_1.start.0, end_2);

            // If the y-coordinates or x-coordinates are more than 2 apart,
            // then they can't possibly share a gear in-between them. So we
            // can skip the rest of the loop and save doing that computation.
            // This turns out to save a *lot* of work.
            if (y_diff > 2) || (x_diff_left > 2) || (x_diff_right > 2) {
                continue;
            }

            let &value_1 = positions.get(pos_1).unwrap();
            let &value_2 = positions.get(pos_2).unwrap();

            let nbrs_1 = neighbours_of(grid, pairs[i_1]);
            let nbrs_2 = neighbours_of(grid, pairs[i_2]);
            let shared_nbrs = intersect(&nbrs_1, &nbrs_2);

            for (pos, value) in shared_nbrs {
                if is_gear(&value) {
                    let ratio = value_1 * value_2;
                    gears.insert(pos, ratio);
                }
            }
        }
    }

    gears
}

fn part_1(grid: &Grid<char>, positions: &HashMap<Pos<Pair>, u32>) -> u32 {
    part_numbers(grid, positions).into_iter().sum()
}

fn part_2(grid: &Grid<char>, positions: &HashMap<Pos<Pair>, u32>) -> u32 {
    gear_ratios(grid, positions).values().sum()
}

fn main() {
    let input = include_str!("../../inputs/day_03.txt");
    let grid: Grid<char> =
        input.lines().map(|line| line.chars().collect()).collect();
    let positions = find_numbers_in_grid(&grid);

    println!("{}", part_1(&grid, &positions));
    println!("{}", part_2(&grid, &positions));
}
