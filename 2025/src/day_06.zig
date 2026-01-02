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
        .{ part2, Alloc, Input },
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

/// Day 6, part 2 - what is the grand total found by adding together all of
/// the answers to the individual problems?
pub fn part2(alloc: std.mem.Allocator, input: []const u8) !u64 {
    var probs = try Problems.parseColumns(alloc, input);
    defer probs.deinit(alloc);

    var total: u64 = 0;
    for (probs._0.items) |prob| {
        total += prob.solve();
    }

    return total;
}

test part2 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(3263827, part2(alloc, example));
}

/// A numeric (binary) operator.
const Op = union(enum) {
    Add,
    Mul,

    const Self = @This();

    /// Parse from corresponding character.
    fn read(char: u8) !Self {
        switch (char) {
            '+' => return .Add,
            '*' => return .Mul,
            else => return error.UnexpectedOp,
        }
    }
};

/// A problem: a list of numbers and an operator `Op`.
const Problem = struct {
    numbers: std.ArrayList(u64),
    op: Op,

    const Self = @This();

    /// Solve this problem.
    fn solve(self: Self) u64 {
        switch (self.op) {
            .Add => {
                var sum: u64 = 0;
                for (self.numbers.items) |n| sum += n;
                return sum;
            },
            .Mul => {
                var prod: u64 = 1;
                for (self.numbers.items) |n| prod *= n;
                return prod;
            },
        }
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
            prob.op = try Op.read(op[0]);
        }
        if (ops.next()) |_| return error.TooManyOps;

        return self;
    }

    /// Parse problems column-wise.
    fn parseColumns(alloc: std.mem.Allocator, text: []const u8) !Self {
        var self: Self = .{ ._0 = .empty };
        errdefer self.deinit(alloc);

        // Populate `columns` so that each entry is a column in the input text.
        var columns: std.ArrayList(std.ArrayList(u8)) = .empty;
        defer {
            for (columns.items) |*row| row.deinit(alloc);
            columns.deinit(alloc);
        }

        var lines = std.mem.tokenizeScalar(u8, text, '\n');
        const first_line = lines.peek() orelse return error.NoInputLines;
        for (first_line) |_| try columns.append(alloc, .empty);

        while (lines.next()) |line| {
            if (line.len != first_line.len) return error.UnevenLines;
            for (line, 0..) |char, i| {
                try columns.items[i].append(alloc, char);
            }
        }

        // Now we are ready for the main loop - each iteration we parse
        // one problem.
        var i: usize = 0;
        while (i < columns.items.len) : (i += 1) {
            // We can read off the operator from the first column.
            const head = columns.items[i];
            const len = head.items.len;
            const op = try Op.read(head.items[len - 1]);

            var problem: Problem = .{ .numbers = .empty, .op = op };
            errdefer problem.numbers.deinit(alloc);

            // Parse each number in this problem, one at a time.
            while (i < columns.items.len) : (i += 1) {
                const row = columns.items[i];
                if (std.mem.allEqual(u8, row.items, ' ')) break;

                const slice = row.items[0 .. row.items.len - 1];
                const trimmed = std.mem.trim(u8, slice, &.{' '});
                const num = try std.fmt.parseInt(u64, trimmed, 10);

                try problem.numbers.append(alloc, num);
            }

            try self._0.append(alloc, problem);
        }

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

test "Problems.parseColumns" {
    const alloc = std.testing.allocator;
    const Prob = struct { numbers: []const u64, op: Op };
    const problems = [_]Prob{
        .{ .numbers = &.{ 1, 24, 356 }, .op = .Mul },
        .{ .numbers = &.{ 369, 248, 8 }, .op = .Add },
        .{ .numbers = &.{ 32, 581, 175 }, .op = .Mul },
        .{ .numbers = &.{ 623, 431, 4 }, .op = .Add },
    };

    var probs = try Problems.parseColumns(alloc, example);
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
    "123 328  51 64 \n" ++
    " 45 64  387 23 \n" ++
    "  6 98  215 314\n" ++
    "*   +   *   +  \n";
