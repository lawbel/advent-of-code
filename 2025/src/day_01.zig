//! Advent of Code 2025, Day 1: Secret Entrance.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Input = utils.Input;

/// Run both parts for day 1.
pub fn main() !void {
    try runDay(
        .{ .day = 1 },
        .{ part1, Input },
        .{ part2, Input },
    );
}

/// Day 1, part 1 - analyze the rotations in your attached document.
/// What's the actual password to open the door?
pub fn part1(input: []const u8) !u32 {
    var dial: i32 = 50;
    var zeroes: u32 = 0;

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len < 1) continue;

        const num = try std.fmt.parseInt(i32, line[1..], 10);
        switch (line[0]) {
            'L' => dial = @mod(dial - num, 100),
            'R' => dial = @mod(dial + num, 100),
            else => return error.MalformedInputLine,
        }

        if (dial == 0) zeroes += 1;
    }

    return zeroes;
}

test part1 {
    const pwd = try part1(example);
    try std.testing.expectEqual(3, pwd);
}

/// Day 1, part 2 - using password method 0x434C49434B, what is the
/// password to open the door?
pub fn part2(input: []const u8) !u32 {
    var dial: i32 = 50;
    var zeroes: u32 = 0;

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len < 1) continue;

        const num = try std.fmt.parseInt(i32, line[1..], 10);
        const new = switch (line[0]) {
            'L' => dial - num,
            'R' => dial + num,
            else => return error.MalformedInputLine,
        };

        if (dial > 0 and new <= 0) zeroes += 1;
        const rotations = @divTrunc(new, 100);
        zeroes += @abs(rotations);
        dial = @mod(new, 100);
    }

    return zeroes;
}

test part2 {
    const pwd = try part2(example);
    try std.testing.expectEqual(6, pwd);
}

const example: []const u8 =
    \\L68
    \\L30
    \\R48
    \\L5
    \\R60
    \\L55
    \\L1
    \\L99
    \\R14
    \\L82
;
