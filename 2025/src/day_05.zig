//! Advent of Code 2025, Day 5: Cafeteria.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Alloc = utils.Alloc;
const Input = utils.Input;

/// Run both parts for day 5.
pub fn main() !void {
    try runDay(
        .{ .day = 5 },
        .{ part1, Alloc, Input },
        .{ part2, Alloc, Input },
    );
}

/// Day 5, part 1 - how many of the available ingredient IDs are fresh?
pub fn part1(alloc: std.mem.Allocator, input: []const u8) !u64 {
    var ingredients = try Ingredients.parse(alloc, input);
    defer ingredients.deinit(alloc);

    ingredients.fresh.compact();
    ingredients.fresh.sort();

    var fresh: u64 = 0;
    for (ingredients.available.items) |ingr| {
        if (ingredients.fresh.binarySearch(ingr)) |_| {
            fresh += 1;
        }
    }

    return fresh;
}

test part1 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(3, part1(alloc, example));
}

/// Day 5, part 2 - how many ingredient IDs are considered to be fresh
/// according to the fresh ingredient ID ranges?
pub fn part2(alloc: std.mem.Allocator, input: []const u8) !u64 {
    var ingredients = try Ingredients.parse(alloc, input);
    defer ingredients.deinit(alloc);

    ingredients.fresh.compact();
    ingredients.fresh.sort();

    var fresh: u64 = 0;
    for (ingredients.fresh._0.items) |range| {
        fresh += range.max - range.min + 1;
    }

    return fresh;
}

test part2 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(14, part2(alloc, example));
}

/// A range (inclusive) of integers `[min, max]`.
const Range = struct {
    min: u64,
    max: u64,

    const Self = @This();

    /// If the two ranges can be merged into one, returns that larger `Range`.
    /// Otherwise, returns `null`.
    ///
    /// Note: because we are dealing with integer ranges, we can merge
    /// e.g. `[1, 10]` with `[11, 20]` to get `[1, 20]` - a difference of 1
    /// at the boundaries is okay.
    fn merge(self: Self, other: Self) ?Self {
        const self_lesser = Range.lessThan({}, self, other);
        const left = if (self_lesser) self else other;
        const right = if (self_lesser) other else self;

        if (right.min <= left.max + 1) {
            return .{
                .min = left.min,
                .max = @max(left.max, right.max),
            };
        } else {
            return null;
        }
    }

    /// Is `self` less than `other`? Takes an unused `void` parameter so that
    /// this method has the right signature to be used by sorting methods
    /// such as `std.sort.insertion`.
    fn lessThan(_: void, self: Self, other: Self) bool {
        return switch (std.math.order(self.min, other.min)) {
            .lt => true,
            .eq => self.max < other.max,
            .gt => false,
        };
    }

    /// Returns `.eq` if `value` is within the range, `.lt` if it's to the
    /// left and `.gt` if it's off to the right. This function is suitable for
    /// use with search methods such as `std.sort.binarySearch`.
    fn compare(value: u64, self: Self) std.math.Order {
        if (value < self.min) {
            return .lt;
        } else if (value > self.max) {
            return .gt;
        } else {
            return .eq;
        }
    }
};

/// A collection of `Range`s.
const Ranges = struct {
    _0: std.ArrayList(Range),

    const Self = @This();
    const empty: Self = .{ ._0 = .empty };

    /// Free associated memory.
    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        self._0.deinit(alloc);
    }

    /// Merge together the ranges with `Range.merge`, until there are no more
    /// merges possible and each `Range` is totally distinct from every other.
    fn compact(self: *Self) void {
        var size: usize = self._0.items.len;
        var left: usize = 0;

        // We attempt to merge `left` with every range to its right, going
        // back to the start on success to retry each merge. Only once no
        // more merges are possible do we increment `left` and repeat.
        outer: while (left < size) {
            var right: usize = left + 1;

            while (right < size) : (right += 1) {
                const l = self._0.items[left];
                const r = self._0.items[right];
                const merged = l.merge(r) orelse continue;

                self._0.items[left] = merged;
                _ = self._0.swapRemove(right);
                size -= 1;
                continue :outer;
            }

            left += 1;
        }
    }

    /// Sort ranges with `Range.lessThan`.
    fn sort(self: *Self) void {
        std.sort.heap(Range, self._0.items, {}, Range.lessThan);
    }

    /// Perform a binary search for `value`, returning the index of the
    /// `Range` containing it or `null` if there is no such range. The ranges
    /// should be compacted and sorted before calling this method, or results
    /// may be nonsensical.
    fn binarySearch(self: Self, value: u64) ?usize {
        return std.sort.binarySearch(
            Range,
            self._0.items,
            value,
            Range.compare,
        );
    }
};

test "Ranges.compact" {
    const alloc = std.testing.allocator;
    var ingr = try Ingredients.parse(alloc, example);
    defer ingr.deinit(alloc);

    ingr.fresh.compact();
    try std.testing.expectEqualSlices(
        Range,
        &.{
            .{ .min = 3, .max = 5 },
            .{ .min = 10, .max = 20 },
        },
        ingr.fresh._0.items,
    );
}

test "Ranges.sort" {
    const alloc = std.testing.allocator;
    var ingr = try Ingredients.parse(alloc, example);
    defer ingr.deinit(alloc);

    ingr.fresh.sort();
    try std.testing.expectEqualSlices(
        Range,
        &.{
            .{ .min = 3, .max = 5 },
            .{ .min = 10, .max = 14 },
            .{ .min = 12, .max = 18 },
            .{ .min = 16, .max = 20 },
        },
        ingr.fresh._0.items,
    );
}

test "Ranges.binarySearch" {
    const alloc = std.testing.allocator;
    var ingr = try Ingredients.parse(alloc, example);
    defer ingr.deinit(alloc);

    ingr.fresh.compact();
    ingr.fresh.sort();

    try std.testing.expectEqual(null, ingr.fresh.binarySearch(1));
    try std.testing.expectEqual(0, ingr.fresh.binarySearch(5));
    try std.testing.expectEqual(null, ingr.fresh.binarySearch(8));
    try std.testing.expectEqual(1, ingr.fresh.binarySearch(11));
    try std.testing.expectEqual(1, ingr.fresh.binarySearch(17));
    try std.testing.expectEqual(null, ingr.fresh.binarySearch(32));
}

/// A collection of fresh ingredient ranges, and a list of available
/// ingredients.
const Ingredients = struct {
    fresh: Ranges,
    available: std.ArrayList(u64),

    const Self = @This();

    /// Parse from string. Caller owns returned memory.
    fn parse(alloc: std.mem.Allocator, input: []const u8) !Self {
        var lines = std.mem.splitScalar(u8, input, '\n');
        var self: Self = .{ .fresh = .empty, .available = .empty };
        errdefer self.deinit(alloc);

        // Process ranges, until we hit an empty line.
        while (lines.next()) |line| {
            if (line.len == 0) break;

            var parts = std.mem.splitScalar(u8, line, '-');
            const min = parts.next() orelse return error.NoRangeStart;
            const max = parts.next() orelse return error.NoRangeStop;
            if (parts.next() != null) return error.ExtraRangePart;

            const range: Range = .{
                .min = try std.fmt.parseInt(u64, min, 10),
                .max = try std.fmt.parseInt(u64, max, 10),
            };
            if (range.min > range.max) return error.BackwardsRange;

            try self.fresh._0.append(alloc, range);
        }

        // Now process ID numbers until end of input or empty line.
        while (lines.next()) |line| {
            if (line.len == 0) break;

            const id = try std.fmt.parseInt(u64, line, 10);
            try self.available.append(alloc, id);
        }

        return self;
    }

    /// Free associated memory.
    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        self.fresh.deinit(alloc);
        self.available.deinit(alloc);
    }
};

test "Ingredients.parse" {
    const alloc = std.testing.allocator;
    var ingr = try Ingredients.parse(alloc, example);
    defer ingr.deinit(alloc);

    try std.testing.expectEqualSlices(
        Range,
        &.{
            .{ .min = 3, .max = 5 },
            .{ .min = 10, .max = 14 },
            .{ .min = 16, .max = 20 },
            .{ .min = 12, .max = 18 },
        },
        ingr.fresh._0.items,
    );
    try std.testing.expectEqualSlices(
        u64,
        &.{ 1, 5, 8, 11, 17, 32 },
        ingr.available.items,
    );
}

/// Our example input for day 5.
const example: []const u8 =
    \\3-5
    \\10-14
    \\16-20
    \\12-18
    \\
    \\1
    \\5
    \\8
    \\11
    \\17
    \\32
;
