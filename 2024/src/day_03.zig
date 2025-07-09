//! Advent of Code 2024, Day 3: Mull It Over.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 3.
pub fn main() !void {
    try utils.mainDay(3, part1, part2);
}

/// Day 3, part 1.
pub fn part1(_: std.mem.Allocator, memory: []const u8) !u64 {
    return sumValidMulExprs(memory, .Ignore);
}

/// Day 3, part 2.
pub fn part2(_: std.mem.Allocator, memory: []const u8) !u64 {
    return sumValidMulExprs(memory, .Handle);
}

/// Iterate over every `MulExpr` in the given `memory`, calculating the result
/// of the multiplication and adding it to a running total.
fn sumValidMulExprs(memory: []const u8, comptime cond: Conditionals) u64 {
    var muls = validMulExprs(memory, cond);
    var total: u64 = 0;

    while (muls.next()) |mul| {
        total += mul.left * mul.right;
    }

    return total;
}

test part1 {
    const total = sumValidMulExprs(example_mem_1, .Ignore);
    try std.testing.expectEqual(161, total);
}

test part2 {
    const total = sumValidMulExprs(example_mem_2, .Handle);
    try std.testing.expectEqual(48, total);
}

/// A multiplication expression like `mul(1,2)`.
const MulExpr = struct { left: u64, right: u64 };

/// Whether or not to handled `do()` and `don't()` expressions.
const Conditionals = union(enum) { Handle, Ignore };

/// An iterable of `MulExpr`s.
fn MulIterator(comptime cond: Conditionals) type {
    return struct {
        const Self = @This();

        memory: []const u8,
        index: ?usize,
        enabled: switch (cond) {
            .Handle => bool,
            .Ignore => void,
        },

        /// Get the next `MulExpr` (if any).
        fn next(self: *Self) ?MulExpr {
            const len = self.memory.len;
            const end = len - 1;
            const indexOf = std.mem.indexOfScalarPos;
            const parseUnsigned = std.fmt.parseUnsigned;

            // Main loop: iterate over the string from starting position
            // `self.index`, moving forwards until we find a `MulExpr` or we
            // run out of string.
            while (self.index) |*index| {
                // Check array bounds.
                const i = index.*;
                if (i > end) break;

                // If handling conditions, check for `do()` and `don't()`.
                if (cond == .Handle) {
                    const do = "do()";
                    if (i + do.len > len) break;
                    const do_str = self.memory[i .. i + do.len];
                    if (std.mem.eql(u8, do, do_str)) {
                        self.enabled = true;
                        index.* += do.len;
                        continue;
                    }

                    const dont = "don't()";
                    if (i + dont.len > len) break;
                    const dont_str = self.memory[i .. i + dont.len];
                    if (std.mem.eql(u8, dont, dont_str)) {
                        self.enabled = false;
                        index.* += dont.len;
                        continue;
                    }

                    if (!self.enabled) {
                        index.* += 1;
                        continue;
                    }
                }

                // Start with 'mul(' string.
                index.* += 1;
                const mul = "mul(";
                if (i + mul.len > len) continue;
                const start = self.memory[i .. i + mul.len];
                if (!std.mem.eql(u8, mul, start)) continue;
                index.* += start.len - 1;

                // Follow with '...,' string, 1-3 chars before ',' allowed.
                if (i + mul.len + 4 > len) continue;
                const first = self.memory[i .. i + mul.len + 4];
                const comma = indexOf(u8, first, mul.len, ',') orelse continue;
                const num_l = self.memory[i + mul.len .. i + comma];
                const left = parseUnsigned(u64, num_l, 10) catch continue;
                index.* += num_l.len + 1;

                // End with '...)' string, 1-3 chars before ')' allowed.
                const close_paren = @min(i + comma + 4, end) + 1;
                if (close_paren > len) continue;
                const second = self.memory[i..close_paren];
                const close = indexOf(u8, second, comma, ')') orelse continue;
                const num_r = self.memory[i + comma + 1 .. i + close];
                const right = parseUnsigned(u64, num_r, 10) catch continue;
                index.* += num_r.len + 1;

                // If we succeeded, return the `MulExpr` leaving `self` ready
                // for the next iteration.
                return .{ .left = left, .right = right };
            }

            // If we got here, we ran through the whole string without finding
            // any `MulExpr`. So, exhaust this iterator and return nothing.
            self.index = null;
            return null;
        }
    };
}

/// Iterate over all valid `MulExpr`s in the given memory.
fn validMulExprs(
    memory: []const u8,
    comptime cond: Conditionals,
) MulIterator(cond) {
    return .{
        .memory = memory,
        .index = 0,
        .enabled = switch (cond) {
            .Handle => true,
            .Ignore => {},
        },
    };
}

test "validMulExprs(.Ignore)" {
    const alloc = std.testing.allocator;
    const expected = [_]MulExpr{
        .{ .left = 2, .right = 4 },
        .{ .left = 5, .right = 5 },
        .{ .left = 11, .right = 8 },
        .{ .left = 8, .right = 5 },
    };

    var muls = std.ArrayListUnmanaged(MulExpr).empty;
    defer muls.deinit(alloc);

    var iter = validMulExprs(example_mem_1, .Ignore);
    while (iter.next()) |expr| {
        try muls.append(alloc, expr);
    }

    try std.testing.expectEqualSlices(MulExpr, &expected, muls.items);
}

test "validMulExprs(.Handle)" {
    const alloc = std.testing.allocator;
    const expected = [_]MulExpr{
        .{ .left = 2, .right = 4 },
        .{ .left = 8, .right = 5 },
    };

    var muls = std.ArrayListUnmanaged(MulExpr).empty;
    defer muls.deinit(alloc);

    var iter = validMulExprs(example_mem_2, .Handle);
    while (iter.next()) |expr| {
        try muls.append(alloc, expr);
    }

    try std.testing.expectEqualSlices(MulExpr, &expected, muls.items);
}

/// The example for day 3, part 1.
const example_mem_1: []const u8 =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)" ++
    "+mul(32,64]then(mul(11,8)mul(8,5))";

/// The example for day 3, part 2.
const example_mem_2: []const u8 =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)" ++
    "+mul(32,64](mul(11,8)undo()?mul(8,5))";
