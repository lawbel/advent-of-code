//! Advent of Code 2024, Day 10: Hoof It.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 10.
pub fn main() !void {
    try utils.mainDay(10, part1, part2);
}

/// Day 10, part 1 - [...].
pub fn part1(_: std.mem.Allocator, _: []const u8) !u64 {
    return 0;
}

/// Day 10, part 2 - [...].
pub fn part2(_: std.mem.Allocator, _: []const u8) !u64 {
    return 0;
}

fn parseMap(
    alloc: std.mem.Allocator,
    text: []const u8,
) !utils.GridUnmanaged(u8) {
    var width: ?usize = null;
    var lines = std.mem.tokenizeScalar(u8, text, '\n');
    var grid: utils.GridUnmanaged(u8) = .empty;
    errdefer grid.deinit(alloc);

    while (lines.next()) |line| {
        const len = line.len;
        if (width) |w| {
            if (len != w) return error.UnevenRows;
        } else {
            width = len;
        }

        var list: std.ArrayListUnmanaged(u8) = try .initCapacity(alloc, len);
        errdefer list.deinit(alloc);

        for (line) |char| {
            const num = try std.fmt.parseUnsigned(u8, &.{char}, 10);
            try list.append(alloc, num);
        }
        try grid._0.append(alloc, list);
    }

    return grid;
}

const Dir = enum { North, East, South, West };

fn peaks(
    alloc: std.mem.Allocator,
    map: utils.GridUnmanaged(u8),
) !struct {
    value: u8,
    positions: std.ArrayListUnmanaged(utils.Coord(usize)),
} {
    _ = alloc;
    _ = map;

    return error.TODO;
}

fn toClosestPeak(
    alloc: std.mem.Allocator,
    map: utils.GridUnmanaged(u8),
) !utils.GridUnmanaged(?Dir) {
    // _ = map;

    const new: utils.GridUnmanaged(?Dir) =
        try .initRowCapacity(alloc, map._0.items.len);

    for (map._0.items) |row| {
        for (row.items) |n| {
            _ = n;
        }
    }

    return new;
}

// test parseMap {
//     const alloc = std.testing.allocator;
//     const example =
//         \\0123
//         \\1234
//         \\8765
//         \\9876
//     ;
//
//     var map = try parseMap(alloc, example);
//     defer map.deinit(alloc);
//
//     try std.testing.expectEqual(4, map._0.items.len);
// }
