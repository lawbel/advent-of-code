//! Advent of Code 2024, Day 7: Bridge Repair.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 7.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;
    const stdout = std.io.getStdOut().writer();
    const equations = try utils.getInputFile(alloc, 7);
    defer alloc.free(equations);

    const result1 = try part1(alloc, equations);
    try stdout.print("part 1: {d}\n", .{result1});

    const result2 = try part2(alloc, equations);
    try stdout.print("part 2: {d}\n", .{result2});
}

/// Day 7, part 1. Parse the given equations, and work out which ones are
/// solvable using addition and multiplication. For those equations, sum up
/// the target values and return the total.
pub fn part1(alloc: std.mem.Allocator, text: []const u8) !u64 {
    var equations = try Equations(u64).parse(alloc, text);
    defer equations.deinit(alloc);
    return calibration(alloc, u64, equations, AddMul);
}

/// Day 7, part 2. Similar to `part1`, but allow concatenation as well as
/// addition and multiplication.
pub fn part2(alloc: std.mem.Allocator, text: []const u8) !u64 {
    var equations = try Equations(u64).parse(alloc, text);
    defer equations.deinit(alloc);
    return calibration(alloc, u64, equations, AddMulCat);
}

/// Calculate the total calibration result for the given equations.
pub fn calibration(
    alloc: std.mem.Allocator,
    comptime T: type,
    equations: Equations(T),
    comptime Op: type,
) !u64 {
    var total: u64 = 0;

    for (equations.in.items) |equation| {
        var solution = try equation.solution(alloc, Op);
        if (solution) |*list| {
            list.deinit(alloc);
            total += equation.target;
        }
    }

    return total;
}

/// Iterate over every possible combination of `values` of size `len`. For
/// example, `combinations(alloc, u8, "ab", 2)` yields
/// `.{ "aa", "ba", "ab", "bb" }`.
///
/// The returned iterator needs freeing with `.deinit(alloc)`.
fn combinations(
    alloc: std.mem.Allocator,
    comptime T: type,
    values: []const T,
    len: usize,
) !CombinationIter(T) {
    if (values.len == 0) return error.EmptyValues;

    var buffer: std.ArrayListUnmanaged(T) = .empty;
    errdefer buffer.deinit(alloc);
    var indices: std.ArrayListUnmanaged(usize) = .empty;
    errdefer indices.deinit(alloc);

    try buffer.appendNTimes(alloc, values[0], len);
    try indices.appendNTimes(alloc, 0, len);

    return .{
        .values = values,
        .buffer = buffer,
        .indices = indices,
    };
}

/// An iterator that yields slices `[]const T` for every combination of the
/// given values of the given length.
fn CombinationIter(comptime T: type) type {
    return struct {
        const Self = @This();

        /// Possible values for each element.
        values: []const T,

        /// Backing memory, mutated on each call to `next`.
        buffer: std.ArrayListUnmanaged(T),

        /// Mirrors the contents of buffer, except that it stores the
        /// index `i` rather than the element `values[i]` at that position.
        indices: std.ArrayListUnmanaged(usize),

        /// Helper var for book-keeping - makes it easy to return the
        /// original buffer without mutation on the first call to `next`.
        first: bool = true,

        /// Free memory associated with this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.buffer.deinit(alloc);
            self.indices.deinit(alloc);
        }

        /// Reset the iterator to its initial state.
        fn reset(self: *Self) void {
            self.first = true;
            for (self.buffer.items) |*val| val.* = self.values[0];
            for (self.indices.items) |*i| i.* = 0;
        }

        /// Yield the next iteration (if any).
        fn next(self: *Self) ?[]const T {
            // On first call of `next`, simply return the buffer unchanged.
            if (self.first) {
                self.first = false;
                return self.buffer.items;
            }

            // On all subsequent calls, we do essentially a carry add:
            //
            // * check position zero;
            // * if that would roll over, check position one;
            // * if that would roll over, check position two;
            //
            // and so on, going as far along the buffer as needed or as
            // possible.
            const limit: usize = self.values.len - 1;
            const len: usize = self.indices.items.len;

            var i: usize = 0;
            while (i < len) : (i += 1) {
                if (self.indices.items[i] < limit) break;
            }

            // If we got to the end of the list, then every single position is
            // maxed out. So there are no more iterations to yield.
            if (i == len) return null;

            // Otherwise, we stopped part-way through the list. Every element
            // before position `i` needs zero-ing out. Position `i` itself
            // is not maxed out yet, so simply increment it by one.
            const cur = self.indices.items[i];
            self.indices.items[i] = cur + 1;
            self.buffer.items[i] = self.values[cur + 1];

            for (0..i) |j| {
                self.indices.items[j] = 0;
                self.buffer.items[j] = self.values[0];
            }

            // Return a view into the current state of the buffer.
            return self.buffer.items;
        }
    };
}

test combinations {
    const alloc = std.testing.allocator;
    const expected = [_][]const u8{
        "000",
        "100",
        "010",
        "110",
        "001",
        "101",
        "011",
        "111",
    };

    var iter = try combinations(alloc, u8, "01", 3);
    defer iter.deinit(alloc);

    for (expected) |exp| {
        const actual = iter.next() orelse return error.ShortIter;
        try std.testing.expectEqualStrings(exp, actual);
    }
    try std.testing.expectEqual(null, iter.next());
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

        /// Suppose that we name the list elements like so:
        ///
        /// * `self.numbers = [i0, i1, ..., iN]`
        /// * `ops          =     [o1, ..., oN]`
        ///
        /// where each `o1, o2, ...` represents either addition `(+)` or
        /// multiplication `(*)`. Then this function returns the result
        ///
        /// ```
        /// (((i0 `o1` i1) `o2` i2) ... `oN` iN)
        /// ```
        fn eval(self: Self, comptime Op: type, ops: []const Op) ?T {
            if (self.numbers.items.len != ops.len + 1) return null;
            if (self.numbers.items.len == 0) return 0;

            var result: T = self.numbers.items[0];
            for (self.numbers.items[1..], ops) |number, op| {
                result = op.apply(T, result, number);
            }
            return result;
        }

        /// Returns a valid solution (if any exists) to this equation. If there
        /// are multiple solutions, returns the first one it finds in no
        /// particular order.
        fn solution(
            self: Self,
            alloc: std.mem.Allocator,
            comptime Op: type,
        ) !?std.ArrayListUnmanaged(Op) {
            const len = self.numbers.items.len;
            if (len == 0) return null;

            const all_ops: []const Op = init: {
                const fields = @typeInfo(Op).@"union".fields;
                var ops: [fields.len]Op = undefined;
                inline for (fields, 0..) |field, i| {
                    ops[i] = @unionInit(Op, field.name, {});
                }
                break :init &ops;
            };

            var iter = try combinations(alloc, Op, all_ops, len - 1);
            defer iter.deinit(alloc);

            while (iter.next()) |ops| {
                if (self.eval(Op, ops) == self.target) {
                    var list: std.ArrayListUnmanaged(Op) = .empty;
                    errdefer list.deinit(alloc);

                    try list.appendSlice(alloc, ops);
                    return list;
                }
            }

            return null;
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

test "Equation.solution(AddMul)" {
    const alloc = std.testing.allocator;
    const Int = i64;
    const soluble = [_]Int{ 190, 3267, 292 };
    const result: Int = 3749;

    var equations = try Equations(Int).parse(alloc, example_equations);
    defer equations.deinit(alloc);

    var total: Int = 0;
    for (equations.in.items) |equation| {
        var solution = try equation.solution(alloc, AddMul);
        defer if (solution) |*list| list.deinit(alloc);

        const actual: bool = (solution != null);
        const expected: bool =
            std.mem.indexOfScalar(Int, &soluble, equation.target) != null;
        try std.testing.expectEqual(expected, actual);

        if (solution != null) total += equation.target;
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
                const shift: T = std.math.log10_int(y) + 1;
                return (x * std.math.pow(T, 10, shift)) + y;
            },
        }
    }
};

test "Equation.solution(AddMulCat)" {
    const alloc = std.testing.allocator;
    const Int = u64;
    const soluble = [_]Int{ 190, 3267, 156, 7290, 192, 292 };
    const result: Int = 11_387;

    var equations = try Equations(Int).parse(alloc, example_equations);
    defer equations.deinit(alloc);

    var total: Int = 0;
    for (equations.in.items) |equation| {
        var solution = try equation.solution(alloc, AddMulCat);
        defer if (solution) |*list| list.deinit(alloc);

        const actual: bool = (solution != null);
        const expected: bool =
            std.mem.indexOfScalar(Int, &soluble, equation.target) != null;
        try std.testing.expectEqual(expected, actual);

        if (solution != null) total += equation.target;
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
