//! Advent of Code 2025, Day 2: Gift Shop.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Input = utils.Input;

/// Run both parts for day 2.
pub fn main() !void {
    try runDay(
        .{ .day = 2 },
        .{ part1, Input },
        .{ part2, Input },
    );
}

/// Day 2, part 1 - what do you get if you add up all of the invalid IDs?
pub fn part1(input: []const u8) !u64 {
    var total: u64 = 0;
    var ranges = Ranges.from(input);

    while (ranges.next()) |range| {
        // Step 1: Try to find a doublet (invalid ID) in the given range,
        // by simple brute force. This could be done a lot smarter, but
        // hopefully the following steps being well-optimised will be enough
        // to give us good perfomance.
        var maybe: ?Doublet = null;
        for (range[0]..range[1] + 1) |n| {
            if (Doublet.from(n)) |dbl| {
                maybe = dbl;
                break;
            }
        }
        var doublet: Doublet = maybe orelse continue;

        // Step 2: we found a doublet, great. Now just increment until we
        // run out of doublets in range.
        while (doublet.value() <= range[1]) {
            total += doublet.value();
            doublet.increment();
        }
    } else |err| switch (err) {
        error.EndOfInput => {},
        else => return err,
    }

    return total;
}

test part1 {
    const result = try part1(example);
    try std.testing.expectEqual(1227775554, result);
}

/// A struct bundling together functions relating to 'doublets' - numbers
/// that are made up of a sequence of digits repeated twice. For example:
/// 11, 22, 1010, 446446, 1188511886.
///
/// The main idea in working with these numbers is to observe that we can
/// characterise them very simply - if you count up one by one
///
/// ```
/// { 1, 2, 3, ... 11, 12, ... 98, 99, 100, 101, ... }
/// ```
///
/// and double up each number in sequence
///
/// ```
/// { 11, 22, 33, ... 1111, 1212, ... 9898, 9999, 100100, 101101, ... }
/// ```
///
/// you get every doublet in ascending order without any gaps.
///
/// With this understanding it becomes easy to e.g. produce the next doublet
/// from an existing one - just increment each half of the doublet by 1.
const Doublet = struct {
    half: u64,

    const Self = @This();

    fn from(num: u64) ?Self {
        const digits = 1 + std.math.log10_int(num);
        if (digits % 2 != 0) return null;
        const base = std.math.pow(u64, 10, digits / 2);
        if (num % base != num / base) return null;
        return .{ .half = num % base };
    }

    fn value(self: Self) u64 {
        const digits = 1 + std.math.log10_int(self.half);
        const base = std.math.pow(u64, 10, digits);
        return (base + 1) * self.half;
    }

    fn increment(self: *Self) void {
        self.half += 1;
    }
};

test "Doublet.from(n) succeeds" {
    const Case = struct { num: u64, half: u64 };
    const cases = [_]Case{
        .{ .num = 5_5, .half = 5 },
        .{ .num = 64_64, .half = 64 },
        .{ .num = 123_123, .half = 123 },
        .{ .num = 11885_11885, .half = 11885 },
    };

    for (cases) |case| {
        const doublet = Doublet.from(case.num);
        try std.testing.expect(doublet != null);
        try std.testing.expectEqual(case.num, doublet.?.value());
        try std.testing.expectEqual(case.half, doublet.?.half);
    }
}

test "Doublet.from(n) fails" {
    const not_doublets = [_]u64{
        7,
        101,
        333,
        50005,
        123_321,
        224_242,
        1698522,
    };

    for (not_doublets) |n| {
        try std.testing.expectEqual(null, Doublet.from(n));
    }
}

test "Doublet.from(n).?.value() == n" {
    for (1..50_000) |n| {
        if (Doublet.from(n)) |doublet| {
            try std.testing.expectEqual(n, doublet.value());
        }
    }
}

/// Day 2, part 2 - [...].
pub fn part2(input: []const u8) !u64 {
    _ = input;
    return 0;
}

test part2 {
    try std.testing.expectEqual(1, 1);
}

const Ranges = struct {
    ranges: std.mem.TokenIterator(u8, .any),

    const Self = @This();

    fn from(input: []const u8) Self {
        return .{ .ranges = std.mem.tokenizeAny(u8, input, ",\n") };
    }

    fn next(self: *Self) !struct { u64, u64 } {
        const range = self.ranges.next() orelse return error.EndOfInput;

        var numbers = std.mem.splitScalar(u8, range, '-');
        const start_s = numbers.next() orelse return error.UnexpectedEOL;
        const stop_s = numbers.next() orelse return error.UnexpectedEOL;
        if (numbers.next() != null) return error.ExtraText;

        const start = try std.fmt.parseInt(u64, start_s, 10);
        const stop = try std.fmt.parseInt(u64, stop_s, 10);
        return .{ start, stop };
    }
};

const example: []const u8 =
    \\11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
    \\1698522-1698528,446443-446449,38593856-38593862,565653-565659,
    \\824824821-824824827,2121212118-2121212124
;
