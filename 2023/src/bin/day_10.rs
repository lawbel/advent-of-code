//! Note: throughout this file, we are using a coordinate system like in
//! graphics programming - so the origin is top-left, positive-x goes to the
//! right and positive-y goes down.

use nom::character::complete as nom_char;
use std::collections::HashSet;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

type Grid<T> = Vec<Vec<T>>;
type Pos = (usize, usize);
type Parser<'a, T> = nom::IResult<&'a str, T>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Obj {
    Pipe(Pipe),
    Ground,
    Start,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Pipe {
    TopLeft,
    TopRight,
    BotLeft,
    BotRight,
    Vertical,
    Horizontal,
}

#[derive(Debug, PartialEq, Eq, EnumIter, PartialOrd, Ord, Clone, Copy)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Obj {
    fn get_pipe(&self) -> Option<&Pipe> {
        match self {
            Obj::Pipe(pipe) => Some(pipe),
            _ => None,
        }
    }
}

impl Direction {
    fn try_step(&self, pos: Pos) -> Option<Pos> {
        match self {
            Direction::Right => Some((pos.0 + 1, pos.1)),
            Direction::Down => Some((pos.0, pos.1 + 1)),
            Direction::Left => {
                let x = pos.0.checked_sub(1)?;
                Some((x, pos.1))
            }
            Direction::Up => {
                let y = pos.1.checked_sub(1)?;
                Some((pos.0, y))
            }
        }
    }
}

fn parse_pipe(input: &str) -> Parser<Pipe> {
    nom::branch::alt((
        nom::combinator::value(Pipe::TopLeft, nom_char::char('F')),
        nom::combinator::value(Pipe::TopRight, nom_char::char('7')),
        nom::combinator::value(Pipe::BotLeft, nom_char::char('L')),
        nom::combinator::value(Pipe::BotRight, nom_char::char('J')),
        nom::combinator::value(Pipe::Vertical, nom_char::char('|')),
        nom::combinator::value(Pipe::Horizontal, nom_char::char('-')),
    ))(input)
}

fn parse_obj(input: &str) -> Parser<Obj> {
    nom::branch::alt((
        nom::combinator::value(Obj::Ground, nom_char::char('.')),
        nom::combinator::value(Obj::Start, nom_char::char('S')),
        nom::combinator::map(parse_pipe, Obj::Pipe),
    ))(input)
}

fn parse_row(input: &str) -> Parser<Vec<Obj>> {
    nom::multi::many1(parse_obj)(input)
}

fn parse_grid(input: &str) -> Parser<(Grid<Obj>, Pos)> {
    let (input, grid) =
        nom::multi::separated_list1(nom_char::newline, parse_row)(input)?;

    let mut starts = Vec::new();
    for (y, row) in grid.iter().enumerate() {
        for (x, val) in row.iter().enumerate() {
            if *val == Obj::Start {
                starts.push((x, y));
            }
        }
    }

    // Check that there is exactly one start. If not, for simplicity we just
    // fail without any information (as that requires more boilerplate).
    match starts[..] {
        [start] => Ok((input, (grid, start))),
        _ => nom::combinator::fail(input),
    }
}

fn next_pipe_in(
    dir: Direction,
    pos: Pos,
    grid: &Grid<Obj>,
) -> Option<(Pos, Direction)> {
    let next_pos = dir.try_step(pos)?;
    let next_pipe = grid_at(next_pos, grid)?.get_pipe()?;

    let next_dir = match (dir, next_pipe) {
        (Direction::Right, Pipe::Horizontal) => Some(Direction::Right),
        (Direction::Right, Pipe::BotRight) => Some(Direction::Up),
        (Direction::Right, Pipe::TopRight) => Some(Direction::Down),

        (Direction::Left, Pipe::Horizontal) => Some(Direction::Left),
        (Direction::Left, Pipe::TopLeft) => Some(Direction::Down),
        (Direction::Left, Pipe::BotLeft) => Some(Direction::Up),

        (Direction::Down, Pipe::Vertical) => Some(Direction::Down),
        (Direction::Down, Pipe::BotLeft) => Some(Direction::Right),
        (Direction::Down, Pipe::BotRight) => Some(Direction::Left),

        (Direction::Up, Pipe::Vertical) => Some(Direction::Up),
        (Direction::Up, Pipe::TopLeft) => Some(Direction::Right),
        (Direction::Up, Pipe::TopRight) => Some(Direction::Left),

        _ => None,
    }?;

    Some((next_pos, next_dir))
}

fn grid_at<T>(pos: Pos, grid: &Grid<T>) -> Option<&T> {
    grid.get(pos.1)?.get(pos.0)
}

fn walk_from(start: Pos, grid: &Grid<Obj>) -> Vec<Pos> {
    let mut positions = Vec::new();
    let mut seen = HashSet::new();
    let mut pos = start;
    let mut dir;

    let mut tmp = None;
    for peek in Direction::iter() {
        if let Some(init) = next_pipe_in(peek, pos, grid) {
            tmp = Some(init.1);
            pos = init.0;
            positions.push(init.0);
            seen.insert(init.0);
            break;
        }
    }

    dir = match tmp {
        None => return positions,
        Some(init) => init,
    };

    while let Some(next) = next_pipe_in(dir, pos, grid) {
        if seen.contains(&next.0) {
            break;
        }
        dir = next.1;
        pos = next.0;
        positions.push(next.0);
        seen.insert(next.0);
    }

    positions
}

fn part_1(start: Pos, grid: &Grid<Obj>) -> usize {
    let loop_size = 1 + walk_from(start, grid).len();
    loop_size / 2
}

fn main() {
    let input = include_str!("../../inputs/day_10.txt");
    let (grid, start) = parse_grid(&input).expect("can parse input").1;

    println!("{}", part_1(start, &grid));
}
