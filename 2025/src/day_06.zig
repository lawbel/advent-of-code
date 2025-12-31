//! Advent of Code 2025, Day 6: Trash Compactor.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Alloc = utils.Alloc;
const Input = utils.Input;

/// Run both parts for day 6.
pub fn main() !void {
    try runDay(
        .{ .day = 6 },
        .{ part1, Alloc, Input },
        .{ part2, Input },
    );
}

/// Day 6, part 1 - what is the grand total found by adding together all of
/// the answers to the individual problems?
pub fn part1(alloc: std.mem.Allocator, input: []const u8) !u64 {
    var probs = try Problems.parse(alloc, input);
    defer probs.deinit(alloc);

    var total: u64 = 0;
    for (probs._0.items) |prob| {
        total += prob.solve();
    }

    return total;
}

test part1 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(4277556, part1(alloc, example));
}

/// Day 6, part 2 - [...]
pub fn part2(input: []const u8) !u64 {
    _ = input;
    return 0;
}

/// A numeric (binary) operator.
const Op = union(enum) { Add, Mul };

/// A problem: a list of numbers and an operator `Op`.
const Problem = struct {
    numbers: std.ArrayList(u64),
    op: Op,

    const Self = @This();

    /// Solve this problem.
    fn solve(self: Self) u64 {
        return switch (self.op) {
            .Add => self.solve_add(),
            .Mul => self.solve_mul(),
        };
    }

    /// Solve an additive problem.
    fn solve_add(self: Self) u64 {
        var result: u64 = 0;
        for (self.numbers.items) |num| {
            result += num;
        }
        return result;
    }

    /// Solve a multiplicative problem.
    fn solve_mul(self: Self) u64 {
        var result: u64 = 1;
        for (self.numbers.items) |num| {
            result *= num;
        }
        return result;
    }
};

/// A list of `Problem`s.
const Problems = struct {
    _0: std.ArrayList(Problem),

    const Self = @This();

    /// Parse a collection of problems from input text.
    fn parse(alloc: std.mem.Allocator, text: []const u8) !Self {
        var lines = std.mem.tokenizeScalar(u8, text, '\n');
        var self: Self = .{ ._0 = .empty };
        errdefer self.deinit(alloc);

        // Initialise each problem.
        const first_line = lines.peek() orelse return error.NoInputLines;
        var problems = std.mem.tokenizeScalar(u8, first_line, ' ');
        while (problems.next()) |_| {
            const prob: Problem = .{ .numbers = .empty, .op = undefined };
            try self._0.append(alloc, prob);
        }

        // Populate each problem with numbers.
        const last_line: []const u8 = while (lines.next()) |line| {
            var tokens = std.mem.tokenizeScalar(u8, line, ' ');

            const first_num = tokens.peek() orelse return error.EmptyLine;
            _ = std.fmt.parseUnsigned(u64, first_num, 10) catch break line;

            for (self._0.items) |*prob| {
                const token = tokens.next() orelse return error.NotEnoughNums;
                const num = try std.fmt.parseUnsigned(u64, token, 10);
                try prob.numbers.append(alloc, num);
            }

            if (tokens.next()) |_| return error.TooManyNums;
        } else {
            return error.NotEnoughLines;
        };

        if (lines.next()) |_| return error.TooManyLines;

        // Populate problem operators.
        var ops = std.mem.tokenizeScalar(u8, last_line, ' ');
        for (self._0.items) |*prob| {
            const op = ops.next() orelse return error.NotEnoughOps;
            if (op.len > 1) return error.MultiCharOp;
            prob.op = switch (op[0]) {
                '+' => .Add,
                '*' => .Mul,
                else => return error.UnexpectedOp,
            };
        }
        if (ops.next()) |_| return error.TooManyOps;

        return self;
    }

    /// Free associated memory.
    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        for (self._0.items) |*prob| prob.numbers.deinit(alloc);
        self._0.deinit(alloc);
    }
};

test "Problems.parse" {
    const alloc = std.testing.allocator;
    const Prob = struct { numbers: []const u64, op: Op };
    const problems = [_]Prob{
        .{ .numbers = &.{ 123, 45, 6 }, .op = .Mul },
        .{ .numbers = &.{ 328, 64, 98 }, .op = .Add },
        .{ .numbers = &.{ 51, 387, 215 }, .op = .Mul },
        .{ .numbers = &.{ 64, 23, 314 }, .op = .Add },
    };

    var probs = try Problems.parse(alloc, example);
    defer probs.deinit(alloc);

    try std.testing.expectEqual(problems.len, probs._0.items.len);
    for (problems, 0..) |expected, i| {
        const actual = probs._0.items[i];
        try std.testing.expectEqual(expected.op, actual.op);
        try std.testing.expectEqualSlices(
            u64,
            expected.numbers,
            actual.numbers.items,
        );
    }
}

/// Our example input for day 6.
const example: []const u8 =
    \\123 328  51 64
    \\ 45 64  387 23
    \\  6 98  215 314
    \\*   +   *   +
;
