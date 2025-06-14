//! Advent of Code 2024, Day 1: Historian Hysteria.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 1.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;
    const stdout = std.io.getStdOut().writer();
    const list = try utils.getInputFile(alloc);
    defer alloc.free(list);

    const val1: u32 = try part1(alloc, list);
    try stdout.print("part 1: {d}\n", .{val1});

    const val2: u32 = try part2(alloc, list);
    try stdout.print("part 2: {d}\n", .{val2});
}

/// Day 1, part 1.
pub fn part1(alloc: std.mem.Allocator, list: []const u8) !u32 {
    // Parse both lists.
    const pair = try parseListPair(alloc, list);
    var left = pair[0];
    var right = pair[1];
    defer left.deinit(alloc);
    defer right.deinit(alloc);

    // Sort both lists in ascending order.
    std.sort.pdq(u32, left.items, {}, std.sort.asc(u32));
    std.sort.pdq(u32, right.items, {}, std.sort.asc(u32));

    // Sum up the distances and return.
    var differences: u32 = 0;

    std.debug.assert(left.items.len == right.items.len);
    for (left.items, right.items) |l, r| {
        differences += if (l < r) r - l else l - r;
    }

    return differences;
}

test part1 {
    const alloc = std.testing.allocator;
    const total_diff = try part1(alloc, example_list);
    try std.testing.expectEqual(total_diff, 11);
}

/// Day 1, part 2.
pub fn part2(alloc: std.mem.Allocator, list: []const u8) !u32 {
    // Parse both lists.
    const pair = try parseListPair(alloc, list);
    var left = pair[0];
    var right = pair[1];
    defer left.deinit(alloc);
    defer right.deinit(alloc);

    // Count numbers in the right list in advance.
    var counts = std.AutoHashMapUnmanaged(u32, u32).empty;
    defer counts.deinit(alloc);

    for (right.items) |r| {
        const entry = try counts.getOrPutValue(alloc, r, 0);
        entry.value_ptr.* += 1;
    }

    // Multiply each entry in `left` by its occurrences in `right`, and sum up.
    var similarity: u32 = 0;

    for (left.items) |l| {
        const count = counts.get(l) orelse 0;
        similarity += l * count;
    }

    return similarity;
}

test part2 {
    const alloc = std.testing.allocator;
    const similarity = try part2(alloc, example_list);
    try std.testing.expectEqual(similarity, 31);
}

/// Parse a pair of lists of numbers. Caller owns both returned lists.
fn parseListPair(
    alloc: std.mem.Allocator,
    list: []const u8,
) !struct {
    std.ArrayListUnmanaged(u32),
    std.ArrayListUnmanaged(u32),
} {
    // Pre-allocate room for the number of lines in the input. Should save us
    // from any need to re-allocate.
    const Vec = std.ArrayListUnmanaged(u32);
    const assumed_len = std.mem.count(u8, list, "\n") + 1;

    var left = try Vec.initCapacity(alloc, assumed_len);
    errdefer left.deinit(alloc);

    var right = try Vec.initCapacity(alloc, assumed_len);
    errdefer right.deinit(alloc);

    // Iterate over each input line.
    var lines = std.mem.splitScalar(u8, list, '\n');
    while (lines.next()) |line| {
        // Skip any blank lines (especially trailing ones).
        if (line.len < 1) continue;

        // We expect a line to have two numbers, separated by whitespace.
        var words = std.mem.tokenizeScalar(u8, line, ' ');

        const word1 = words.next() orelse return error.ShortLine;
        const word2 = words.next() orelse return error.ShortLine;
        std.debug.assert(words.next() == null);

        // Parse the numbers and add them to each list.
        const num1 = try std.fmt.parseInt(u32, word1, 10);
        const num2 = try std.fmt.parseInt(u32, word2, 10);

        try left.append(alloc, num1);
        try right.append(alloc, num2);
    }

    return .{ left, right };
}

test parseListPair {
    const alloc = std.testing.allocator;
    const lefts: [6]u32 = .{ 3, 4, 2, 1, 3, 3 };
    const rights: [6]u32 = .{ 4, 3, 5, 3, 9, 3 };

    var pair = try parseListPair(alloc, example_list);
    defer pair[0].deinit(alloc);
    defer pair[1].deinit(alloc);

    try std.testing.expectEqualSlices(u32, pair[0].items, &lefts);
    try std.testing.expectEqualSlices(u32, pair[1].items, &rights);
}

/// The example for day 1.
const example_list: []const u8 =
    \\3   4
    \\4   3
    \\2   5
    \\1   3
    \\3   9
    \\3   3
;
