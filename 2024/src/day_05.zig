//! Advent of Code 2024, Day 5: Print Queue.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 5.
pub fn main() !void {
    try utils.mainDay(5, part1, part2);
}

/// Day 5, part 1.
pub fn part1(alloc: std.mem.Allocator, text: []const u8) !u64 {
    var manual = try SafetyManual(u64).parse(alloc, text);
    defer manual.deinit(alloc);

    var sum: u64 = 0;
    for (manual.updates.inner.items) |update| {
        if (updateIsLegal(u64, manual.rules, update.items)) {
            const middle = update.items.len / 2;
            sum += update.items[middle];
        }
    }

    return sum;
}

test part1 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(143, part1(alloc, example_manual));
}

/// Day 5, part 2.
pub fn part2(alloc: std.mem.Allocator, text: []const u8) !u64 {
    var manual = try SafetyManual(u64).parse(alloc, text);
    defer manual.deinit(alloc);

    var sum: u64 = 0;
    for (manual.updates.inner.items) |update| {
        if (updateIsLegal(u64, manual.rules, update.items)) {
            continue;
        }

        sortUpdate(u64, manual.rules, update.items);
        const middle = update.items.len / 2;
        sum += update.items[middle];
    }

    return sum;
}

test part2 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(123, part2(alloc, example_manual));
}

/// Shorthand name for a (hash) set of pairs `(T, T)`.
fn Rules(comptime T: type) type {
    return std.AutoHashMapUnmanaged(struct { T, T }, void);
}

/// A safety manual - a collection of rules, and a list of updates. Each
/// update is itself a list of (page) numbers.
fn SafetyManual(comptime T: type) type {
    return struct {
        const Self = @This();

        rules: Rules(T),
        updates: utils.GridUnmanaged(T),

        /// Simple helper method to free associated memory.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.rules.deinit(alloc);
            self.updates.deinit(alloc);
        }

        /// Parse from string.
        fn parse(alloc: std.mem.Allocator, text: []const u8) !Self {
            const int = std.fmt.parseUnsigned;
            var lines = std.mem.splitScalar(u8, text, '\n');

            // Parse the block of rules.
            var rules: Rules(T) = .empty;
            errdefer rules.deinit(alloc);

            while (lines.next()) |line| {
                if (line.len == 0) break;

                var nums = std.mem.splitScalar(u8, line, '|');
                const one = nums.next() orelse return error.ParseError;
                const two = nums.next() orelse return error.ParseError;
                if (nums.next() != null) return error.ParseError;

                const before = int(T, one, 10) catch return error.ParseError;
                const after = int(T, two, 10) catch return error.ParseError;
                try rules.put(alloc, .{ before, after }, {});
            }

            // Parse the block of updates.
            var updates: utils.GridUnmanaged(T) = .{ .inner = .empty };
            errdefer updates.deinit(alloc);

            while (lines.next()) |line| {
                if (line.len == 0) continue;

                var buffer: std.ArrayListUnmanaged(T) = .empty;
                errdefer buffer.deinit(alloc);

                var nums = std.mem.splitScalar(u8, line, ',');
                while (nums.next()) |num| {
                    const val = int(T, num, 10) catch return error.ParseError;
                    try buffer.append(alloc, val);
                }

                try updates.inner.append(alloc, buffer);
            }

            return .{ .rules = rules, .updates = updates };
        }
    };
}

test "SafetyManual.parse" {
    const alloc = std.testing.allocator;
    const Int = u32;
    const rules = [_]struct { Int, Int }{
        .{ 47, 53 }, .{ 97, 13 }, .{ 97, 61 }, .{ 97, 47 }, .{ 75, 29 },
        .{ 61, 13 }, .{ 75, 53 }, .{ 29, 13 }, .{ 97, 29 }, .{ 53, 29 },
        .{ 61, 53 }, .{ 97, 53 }, .{ 61, 29 }, .{ 47, 13 }, .{ 75, 47 },
        .{ 97, 75 }, .{ 47, 61 }, .{ 75, 61 }, .{ 47, 29 }, .{ 75, 13 },
        .{ 53, 13 },
    };
    const updates = [_][]const Int{
        &.{ 75, 47, 61, 53, 29 },
        &.{ 97, 61, 53, 29, 13 },
        &.{ 75, 29, 13 },
        &.{ 75, 97, 47, 61, 53 },
        &.{ 61, 13, 29 },
        &.{ 97, 13, 75, 29, 47 },
    };

    // Populate a safety manual with the above rules and updates.
    var expected: SafetyManual(Int) = .{
        .rules = .empty,
        .updates = .{ .inner = .empty },
    };
    defer expected.deinit(alloc);

    for (rules) |pair| {
        try expected.rules.put(alloc, pair, {});
    }
    for (updates) |update| {
        var list: std.ArrayListUnmanaged(Int) = .empty;
        try list.appendSlice(alloc, update);
        errdefer list.deinit(alloc);
        try expected.updates.inner.append(alloc, list);
    }

    // Parse the same safety manual from our example string.
    var actual = try SafetyManual(Int).parse(alloc, example_manual);
    defer actual.deinit(alloc);

    // Check that the two are equal.
    try std.testing.expectEqual(expected.rules.size, actual.rules.size);
    var keys = expected.rules.keyIterator();
    while (keys.next()) |key| {
        try std.testing.expect(actual.rules.contains(key.*));
    }

    const expected_items = expected.updates.inner.items;
    const actual_items = actual.updates.inner.items;
    try std.testing.expectEqual(expected_items.len, actual_items.len);
    for (expected_items, actual_items) |exp, act| {
        try std.testing.expectEqualSlices(Int, exp.items, act.items);
    }
}

/// Sort the given `update` according to `rules`.
fn sortUpdate(
    comptime T: type,
    rules: Rules(T),
    update: []T,
) void {
    const asc = struct {
        fn inner(rule: *const Rules(T), a: T, b: T) bool {
            const b_less_than_a = rule.contains(.{ b, a });
            return !b_less_than_a;
        }
    }.inner;

    // I figure we should prefer a stable sort. There is a `std.sort.block`
    // with better asymptotics than insertion sort, but it only works on
    // strict comparison operators (that is: `<` but not `<=`). I don't feel
    // satisfied in assuming that based on the rules we have, so insertion
    // will have to do.
    std.sort.insertion(T, update, &rules, asc);
}

test sortUpdate {
    const alloc = std.testing.allocator;
    const Int = u32;

    // It seems to be disallowed to mutate a local slice such as a `[3][]Int`.
    // Instead it must be heap-allocated. We can at-least put the initial
    // values here, and simply copy them over.
    const static = [3][]const Int{
        &.{ 75, 97, 47, 61, 53 },
        &.{ 61, 13, 29 },
        &.{ 97, 13, 75, 29, 47 },
    };
    var updates = [_]std.ArrayListUnmanaged(Int){.empty} ** static.len;
    defer for (&updates) |*update| update.deinit(alloc);
    for (static, 0..) |slice, i| {
        try updates[i].appendSlice(alloc, slice);
    }

    const expected: [static.len][]const Int = .{
        &.{ 97, 75, 47, 61, 53 },
        &.{ 61, 29, 13 },
        &.{ 97, 75, 47, 29, 13 },
    };

    // Now that we have our initial updates able to be sorted (mutated), we
    // will get our rules in the most convenient way - parse the example once
    // again.
    var manual = try SafetyManual(Int).parse(alloc, example_manual);
    defer manual.deinit(alloc);

    // Finally we can do the actual test itself. Sort the above 'static'
    // initial updates, and check if the result is as `expected`.
    for (updates) |update| {
        sortUpdate(Int, manual.rules, update.items);
    }

    for (expected, updates) |expect, actual| {
        try std.testing.expectEqualSlices(Int, expect, actual.items);
    }
}

/// Test whether the update is legal, according to given rules.
///
/// Every rule must be respected - rules look like pairs `(before, after)` and
/// they are upheld if `before` always comes before `after` in the given
/// `update` list. If one or both of `before` and `after` is absent from
/// `update`, then this particular rule is considered to be respected by
/// default.
fn updateIsLegal(
    comptime T: type,
    rules: Rules(T),
    update: []const T,
) bool {
    for (update, 1..) |before, i| {
        for (update[i..]) |after| {
            const is_illegal = rules.contains(.{ after, before });
            if (is_illegal) return false;
        }
    }
    return true;
}

test updateIsLegal {
    const alloc = std.testing.allocator;
    const legality = [_]bool{ true, true, true, false, false, false };

    var manual = try SafetyManual(u32).parse(alloc, example_manual);
    defer manual.deinit(alloc);

    try std.testing.expectEqual(legality.len, manual.updates.inner.items.len);
    for (legality, manual.updates.inner.items) |expected, update| {
        const actual = updateIsLegal(u32, manual.rules, update.items);
        try std.testing.expectEqual(expected, actual);
    }
}

/// The running example for day 5.
const example_manual: []const u8 =
    \\47|53
    \\97|13
    \\97|61
    \\97|47
    \\75|29
    \\61|13
    \\75|53
    \\29|13
    \\97|29
    \\53|29
    \\61|53
    \\97|53
    \\61|29
    \\47|13
    \\75|47
    \\97|75
    \\47|61
    \\75|61
    \\47|29
    \\75|13
    \\53|13
    \\
    \\75,47,61,53,29
    \\97,61,53,29,13
    \\75,29,13
    \\75,97,47,61,53
    \\61,13,29
    \\97,13,75,29,47
;
