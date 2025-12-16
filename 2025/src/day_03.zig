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
    var banks = Banks.from(input);

    while (banks.next()) |bank| {
        const one_pos = std.mem.indexOfMax(u8, bank[0 .. bank.len - 1]);
        const one_char = bank[one_pos];
        const two_char = std.mem.max(u8, bank[one_pos + 1 ..]);

        const one = try std.fmt.charToDigit(one_char, 10);
        const two = try std.fmt.charToDigit(two_char, 10);
        const joltage = (10 * one) + two;

        total += joltage;
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

/// Day 3, part 2 - [...].
pub fn part2(input: []const u8) !u64 {
    _ = input;
    return 0;
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
