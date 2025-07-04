//! Advent of Code 2024, Day 7: Bridge Repair.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 7.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;
    const stdout = std.io.getStdOut().writer();
    const lines = try utils.getInputFile(alloc, 7);
    defer alloc.free(lines);

    const Int = u64;
    var equations = try Equations(Int).parse(alloc, lines);
    defer equations.deinit(alloc);

    try stdout.print("part 1: {d}\n", .{try part1(Int, equations)});
    try stdout.print("part 2: {d}\n", .{try part2(Int, equations)});
}

/// Day 7, part 1. Parse the given equations, and work out which ones are
/// solvable using addition and multiplication. For those equations, sum up
/// the target values and return the total.
pub fn part1(comptime T: type, equations: Equations(T)) !T {
    var total: T = 0;

    for (equations.in.items) |equation| {
        if (equation.hasSolution(AddMul)) {
            total += equation.target;
        }
    }

    return total;
}

/// Day 7, part 2. Similar to `part1`, but allow concatenation as well as
/// addition and multiplication.
pub fn part2(comptime T: type, equations: Equations(T)) !T {
    var total: T = 0;

    for (equations.in.items) |equation| {
        if (equation.hasSolution(AddMulCat)) {
            total += equation.target;
        }
    }

    return total;
}

/// A list of `Equation`s.
fn Equations(comptime T: type) type {
    return struct {
        const Self = @This();
        const Item = Equation(T);

        in: std.ArrayListUnmanaged(Item),

        /// Initialize to an empty list.
        const empty: Self = .{ .in = .empty };

        /// Free associated memory.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            for (self.in.items) |*item| item.deinit(alloc);
            self.in.deinit(alloc);
        }

        /// Parse from a string, where each line is expected to contain a
        /// single equation. Defers to `Equation.parse` for the line-by-line
        /// parsing.
        fn parse(alloc: std.mem.Allocator, lines: []const u8) !Self {
            var list: std.ArrayListUnmanaged(Item) =
                try .initCapacity(alloc, std.mem.count(u8, lines, "\n") + 1);
            errdefer list.deinit(alloc);

            var iter = std.mem.tokenizeScalar(u8, lines, '\n');
            while (iter.next()) |line| {
                const item = try Item.parse(alloc, line);
                try list.append(alloc, item);
            }

            return .{ .in = list };
        }
    };
}

test "Equations.parse" {
    const alloc = std.testing.allocator;
    const Int = u32;
    const static = [_][]const Int{
        &.{ 190, 10, 19 },
        &.{ 3267, 81, 40, 27 },
        &.{ 83, 17, 5 },
        &.{ 156, 15, 6 },
        &.{ 7290, 6, 8, 6, 15 },
        &.{ 161011, 16, 10, 13 },
        &.{ 192, 17, 8, 14 },
        &.{ 21037, 9, 7, 18, 13 },
        &.{ 292, 11, 6, 16, 20 },
    };

    var expected: Equations(Int) = .empty;
    defer expected.deinit(alloc);
    for (static) |slice| {
        var list: std.ArrayListUnmanaged(Int) = .empty;
        errdefer list.deinit(alloc);
        try list.appendSlice(alloc, slice[1..]);
        try expected.in.append(alloc, .{ .target = slice[0], .numbers = list });
    }

    var actual = try Equations(Int).parse(alloc, example_equations);
    defer actual.deinit(alloc);

    try std.testing.expectEqual(expected.in.items.len, actual.in.items.len);
    for (expected.in.items, actual.in.items) |exp, act| {
        try std.testing.expectEqual(exp.target, act.target);
        try std.testing.expectEqualSlices(
            Int,
            exp.numbers.items,
            act.numbers.items,
        );
    }
}

/// An equation representing a target and a list of numbers.
fn Equation(comptime T: type) type {
    return struct {
        const Self = @This();

        target: T,
        numbers: std.ArrayListUnmanaged(T),

        /// Free associated memory.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.numbers.deinit(alloc);
        }

        /// Parse equation from a string like `3267: 81 40 27`.
        fn parse(alloc: std.mem.Allocator, line: []const u8) !Self {
            // Split target and numbers.
            var sides = std.mem.splitScalar(u8, line, ':');
            const left = sides.next() orelse return error.MissingTarget;
            const right = sides.next() orelse return error.MissingNumbers;
            if (sides.next() != null) return error.ExtraColons;

            // Parse target.
            const target = try std.fmt.parseUnsigned(T, left, 10);

            // Ready list for numbers.
            var numbers: std.ArrayListUnmanaged(T) =
                try .initCapacity(alloc, std.mem.count(u8, right, " ") + 1);
            errdefer numbers.deinit(alloc);

            // Parse numbers.
            var words = std.mem.tokenizeAny(u8, right, " \n");
            while (words.next()) |word| {
                const number = try std.fmt.parseUnsigned(T, word, 10);
                try numbers.append(alloc, number);
            }

            return .{ .target = target, .numbers = numbers };
        }

        /// Returns whether or not there exists a valid solution to
        /// this equation. Assumes that all operators `op: Op` increase
        /// their inputs - that `op.apply(T, a, b) >= a` for
        /// any `a, b : T`. Recursively calls `trySolveFrom`, at a max depth
        /// of `self.numbers.items.len`.
        fn hasSolution(self: Self, comptime Op: type) bool {
            return self.trySolveFrom(Op, 0, 0);
        }

        /// Checks for any solution using operators `op: Op` on the remaining
        /// values `self.numbers[index .. ]` to increase the `total` value
        /// until it reaches `self.target`. Assumes that all operators
        /// `op: Op` increase the value of their inputs.
        fn trySolveFrom(
            self: Self,
            comptime Op: type,
            total: T,
            index: usize,
        ) bool {
            if (index == self.numbers.items.len) {
                return (total == self.target);
            }

            const item = self.numbers.items[index];
            inline for (@typeInfo(Op).@"union".fields) |field| {
                const op = @unionInit(Op, field.name, {});
                const next: T = op.apply(T, total, item);
                const over = (next > self.target);
                if (!over and self.trySolveFrom(Op, next, index + 1)) {
                    return true;
                }
            }

            return false;
        }
    };
}

/// Addition `(+)` or multiply `(*)` operator.
const AddMul = union(enum) {
    const Self = @This();

    Add,
    Mul,

    /// Apply this operator to the given values.
    fn apply(self: Self, comptime T: type, x: T, y: T) T {
        return switch (self) {
            .Add => x + y,
            .Mul => x * y,
        };
    }
};

test "Equation.hasSolution(AddMul)" {
    const alloc = std.testing.allocator;
    const Int = i64;
    const soluble = [_]Int{ 190, 3267, 292 };
    const result: Int = 3749;

    var equations = try Equations(Int).parse(alloc, example_equations);
    defer equations.deinit(alloc);

    var total: Int = 0;
    for (equations.in.items) |equation| {
        const index = std.mem.indexOfScalar;
        const expected: bool = index(Int, &soluble, equation.target) != null;
        const actual: bool = equation.hasSolution(AddMul);

        try std.testing.expectEqual(expected, actual);
        if (actual) total += equation.target;
    }

    try std.testing.expectEqual(result, total);
}

/// Addition `(+)` or multiply `(*)` or concatenate `(||)` operator.
const AddMulCat = union(enum) {
    const Self = @This();

    Add,
    Mul,
    Cat,

    /// Apply this operator to the given values.
    fn apply(self: Self, comptime T: type, x: T, y: T) T {
        switch (self) {
            .Add => return x + y,
            .Mul => return x * y,
            .Cat => {
                const shift = std.math.log10_int(y) + 1;
                const tens = std.math.pow(T, 10, shift);
                return (x * tens) + y;
            },
        }
    }
};

test "Equation.hasSolution(AddMulCat)" {
    const alloc = std.testing.allocator;
    const Int = u64;
    const soluble = [_]Int{ 190, 3267, 156, 7290, 192, 292 };
    const result: Int = 11_387;

    var equations = try Equations(Int).parse(alloc, example_equations);
    defer equations.deinit(alloc);

    var total: Int = 0;
    for (equations.in.items) |equation| {
        const index = std.mem.indexOfScalar;
        const expected: bool = index(Int, &soluble, equation.target) != null;
        const actual: bool = equation.hasSolution(AddMulCat);

        try std.testing.expectEqual(expected, actual);
        if (actual) total += equation.target;
    }

    try std.testing.expectEqual(result, total);
}

/// The running example of a set of equations for day 7.
const example_equations: []const u8 =
    \\190: 10 19
    \\3267: 81 40 27
    \\83: 17 5
    \\156: 15 6
    \\7290: 6 8 6 15
    \\161011: 16 10 13
    \\192: 17 8 14
    \\21037: 9 7 18 13
    \\292: 11 6 16 20
;
