//! Advent of Code 2025, Day 3: Lobby.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Input = utils.Input;

/// Run both parts for day 3.
pub fn main() !void {
    try runDay(
        .{ .day = 3 },
        .{ part1, Input },
        .{ part2, Input },
    );
}

/// Day 3, part 1 - find the maximum joltage possible from each bank;
/// what is the total output joltage?
pub fn part1(input: []const u8) !u64 {
    var total: u64 = 0;
    var banks: Banks = .from(input);

    while (banks.next()) |bank| {
        total += try joltage(2, bank);
    } else |err| switch (err) {
        error.EndOfInput => {},
        else => return err,
    }

    return total;
}

test part1 {
    const result = try part1(example);
    try std.testing.expectEqual(357, result);
}

/// Day 3, part 2 - what is the new total output joltage?
pub fn part2(input: []const u8) !u64 {
    var total: u64 = 0;
    var banks: Banks = .from(input);

    while (banks.next()) |bank| {
        total += try joltage(12, bank);
    } else |err| switch (err) {
        error.EndOfInput => {},
        else => return err,
    }

    return total;
}

test part2 {
    const result = try part2(example);
    try std.testing.expectEqual(3121910778619, result);
}

/// Calculate the largest joltage achievable from the given battery bank by
/// turning on the given number of batteries.
fn joltage(batteries: usize, bank: []const u8) !u64 {
    var jolts: u64 = 0;
    var start: usize = 0;

    for (0..batteries) |i| {
        // The best choice is simply the largest digit, preferring earlier
        // digits in case of a tie. We must take care to leave enough room
        // at the end of the bank for any remaining choices.
        const stop: usize = bank.len - batteries + i + 1;
        const offset = std.mem.indexOfMax(u8, bank[start..stop]);

        // Now read that digit as an integer, shift it over to the right
        // 'tens' place, and add it to jolts to set that digit. Don't forget
        // to bump `start` ready for the next iteration in the loop.
        const char = bank[start + offset];
        const digit = try std.fmt.charToDigit(char, 10);
        const tens = digit * std.math.pow(u64, 10, batteries - i - 1);

        start += offset + 1;
        jolts += tens;
    }

    return jolts;
}

/// Iterates over a collection of battery banks.
const Banks = struct {
    _0: std.mem.TokenIterator(u8, .scalar),

    const Self = @This();

    /// Initialise from input string.
    fn from(input: []const u8) Self {
        return .{ ._0 = std.mem.tokenizeScalar(u8, input, '\n') };
    }

    /// Yield the next bank. Returns `error.EndOfInput` once the last bank
    /// has been returned, and `error.InvalidBattery` if the bank is invalid.
    fn next(self: *Self) ![]const u8 {
        const bank = self._0.next() orelse return error.EndOfInput;
        std.debug.assert(bank.len > 0);

        for (bank) |char| {
            if (!std.ascii.isDigit(char)) {
                return error.InvalidBattery;
            }
        }

        return bank;
    }
};

/// Our example input for day 3.
const example: []const u8 =
    \\987654321111111
    \\811111111111119
    \\234234234234278
    \\818181911112111
;
