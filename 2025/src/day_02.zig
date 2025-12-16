//! Advent of Code 2025, Day 2: Gift Shop.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Input = utils.Input;
const Alloc = utils.Alloc;

/// Run both parts for day 2.
pub fn main() !void {
    try runDay(
        .{ .day = 2 },
        .{ part1, Input },
        .{ part2, Alloc, Input },
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
        var doublet: Couplet =
            for (range[0]..range[1] + 1) |n| {
                if (Couplet.from(2, n)) |dbl| {
                    break dbl;
                }
            } else continue;

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

/// Day 2, part 2 - what do you get if you add up all of the
/// invalid IDs using these new rules?.
pub fn part2(alloc: std.mem.Allocator, input: []const u8) !u64 {
    var total: u64 = 0;

    // A collection of IDs; see below comments.
    var ids: std.AutoArrayHashMapUnmanaged(u64, void) = .empty;
    defer ids.deinit(alloc);

    // Loop through each range in `input`.
    var ranges = Ranges.from(input);
    while (ranges.next()) |range| {
        // Reset `ids` to be empty.
        ids.clearRetainingCapacity();

        // What is the largest kind of couplet that could exist in
        // this range? Answer: as many digits as there are in the upper
        // limit of the range.
        const max_size = 1 + std.math.log10_int(range[1]);

        for (2..max_size + 1) |size| {
            // For each size of couplet, try to find a couplet (invalid ID)
            // of that size. If we can't find any, end the loop.
            var couplet: Couplet =
                for (range[0]..range[1] + 1) |n| {
                    if (Couplet.from(size, n)) |cpl| {
                        break cpl;
                    }
                } else continue;

            // Now effeciently increment through the rest of the couplets
            // within this range. We add them to the collection `ids` rather
            // than adding them to `total` right away, to avoid any
            // double-counting. For example, the number 333333 is a doublet
            // of 333 and a triplet of 33, so we could find it twice.
            while (couplet.value() <= range[1]) {
                try ids.put(alloc, couplet.value(), {});
                couplet.increment();
            }
        }

        // Now we've got all our couplets counted just the once, count them up
        // and add to the total.
        for (ids.keys()) |id| {
            total += id;
        }
    } else |err| switch (err) {
        error.EndOfInput => {},
        else => return err,
    }

    return total;
}

test part2 {
    const alloc = std.testing.allocator;
    const result = try part2(alloc, example);
    try std.testing.expectEqual(4174379265, result);
}

/// A struct bundling together functions relating to 'couplets' - numbers
/// that are made up of a sequence of digits repeated two or more times.
/// For example, some doublets are 11, 22, 1010, 446446, 1188511886.
///
/// The main idea in working with these numbers is to observe that we can
/// characterise them very simply. Again take doublets as an example - if
/// you count up one by one
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
///
/// All the above remarks apply just as well to triplets, quadruplets, etc.
/// Doublets are just the special case `const Doublet = Couplet(2)`, to work
/// with triplets instead simply use `const Triplet = Couplet(3)` and so on.
///
/// NOTE: it would be nice to have `fn Couplet(size: comptime_int)` and
/// enable comptime inlining of loops and so on, but we can't do that as we
/// need to accept `size`s that are only known at runtime.
const Couplet = struct {
    sequence: u64,
    size: u64,

    const Self = @This();

    /// Initialise couplet of given size from `number`; returns `null`
    /// if `number` is not actually a valid couplet.
    fn from(size: u64, number: u64) ?Self {
        // The base-10 expansion of `num` should be some multiple
        // of `size` digits long.
        const digits = 1 + std.math.log10_int(number);
        if (digits % size != 0) return null;

        // Now check that each group of `n` digits in `num` is the same.
        const base = std.math.pow(u64, 10, digits / size);
        const seq = number % base;
        var num = number / base;
        for (1..size) |_| {
            if (num % base != seq) return null;
            num /= base;
        }

        // That all checks out, so we have a valid couplet.
        return .{ .size = size, .sequence = seq };
    }

    /// Yield the value of this couplet as a `u64`.
    fn value(self: Self) u64 {
        const digits = 1 + std.math.log10_int(self.sequence);

        // Calculate `total` by filling in each group of `self.size` digits,
        // one after the other.
        var total: u64 = 0;
        for (0..self.size) |i| {
            const factor = std.math.pow(u64, 10, digits * i);
            total += self.sequence * factor;
        }

        return total;
    }

    /// Increment to the next-greatest couplet.
    fn increment(self: *Self) void {
        self.sequence += 1;
    }
};

test "Couplet.from(2, n) succeeds" {
    const Case = struct { num: u64, half: u64 };
    const cases = [_]Case{
        .{ .num = 5_5, .half = 5 },
        .{ .num = 64_64, .half = 64 },
        .{ .num = 123_123, .half = 123 },
        .{ .num = 11885_11885, .half = 11885 },
    };

    for (cases) |case| {
        const doublet = Couplet.from(2, case.num);
        try std.testing.expect(doublet != null);
        try std.testing.expectEqual(case.num, doublet.?.value());
        try std.testing.expectEqual(case.half, doublet.?.sequence);
    }
}

test "Couplet.from(2, n) fails" {
    const not_doublets =
        [_]u64{ 7, 101, 333, 50005, 123_321, 224_242, 1698522 };

    for (not_doublets) |n| {
        try std.testing.expectEqual(null, Couplet.from(2, n));
    }
}

test "Couplet.from(2, n).?.value() == n" {
    for (1..50_000) |n| {
        if (Couplet.from(2, n)) |doublet| {
            try std.testing.expectEqual(n, doublet.value());
        }
    }
}

test "Couplet.from(3+, n) succeeds" {
    try std.testing.expectEqual(
        Couplet{ .size = 3, .sequence = 76 },
        Couplet.from(3, 76_76_76),
    );
    try std.testing.expectEqual(555, Couplet.from(3, 555).?.value());
    try std.testing.expectEqual(8888, Couplet.from(4, 8888).?.value());
    try std.testing.expectEqual(
        Couplet{ .size = 5, .sequence = 123 },
        Couplet.from(5, 123_123_123_123_123),
    );
}

/// An interable of numeric ranges such as `11-22` and `998-1012`.
const Ranges = struct {
    ranges: std.mem.TokenIterator(u8, .any),

    const Self = @This();

    /// Initialise from input text. This `Ranges` should not outlive the
    /// given input text, as it yields slices into it.
    fn from(input: []const u8) Self {
        return .{ .ranges = std.mem.tokenizeAny(u8, input, ",\n") };
    }

    /// Yield the next range. If there are no ranges left,
    /// returns `error.EndOfInput`. If there are errors in parsing the next
    /// range, returns one of a number of different errors.
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

/// Our example input for day 2.
const example: []const u8 =
    \\11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
    \\1698522-1698528,446443-446449,38593856-38593862,565653-565659,
    \\824824821-824824827,2121212118-2121212124
;
