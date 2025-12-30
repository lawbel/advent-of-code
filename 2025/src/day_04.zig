//! Advent of Code 2025, Day 4: Printing Department.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Alloc = utils.Alloc;
const Input = utils.Input;

/// Run both parts for day 4.
pub fn main() !void {
    try runDay(
        .{ .day = 4 },
        .{ part1, Alloc, Input },
        .{ part2, Alloc, Input },
    );
}

/// Day 4, part 1 - how many rolls of paper can be accessed by a forklift?
pub fn part1(alloc: std.mem.Allocator, input: []const u8) !usize {
    var grid = try Grid.parse(alloc, input);
    defer grid.deinit(alloc);
    var points = try accessible(alloc, grid);
    defer points.deinit(alloc);

    return points.items.len;
}

test part1 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(13, part1(alloc, example));
}

/// Day 4, part 2 - how many rolls of paper in total can be removed by the
/// Elves and their forklifts?
pub fn part2(alloc: std.mem.Allocator, input: []const u8) !usize {
    var grid = try Grid.parse(alloc, input);
    defer grid.deinit(alloc);

    var removed: usize = 0;
    while (true) {
        var list = try accessible(alloc, grid);
        defer list.deinit(alloc);

        if (list.items.len == 0) break;

        removed += list.items.len;
        for (list.items) |pt| {
            grid._0.items[pt.y].items[pt.x] = empty;
        }
    }

    return removed;
}

test part2 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(43, part2(alloc, example));
}

/// Which rolls of paper are accessible - adjacent tiles occupied with another
/// roll of paper is less than `limit`? Caller owns returned memory.
pub fn accessible(
    alloc: std.mem.Allocator,
    grid: Grid,
) !std.ArrayListUnmanaged(Point) {
    var list: std.ArrayListUnmanaged(Point) = .empty;
    errdefer list.deinit(alloc);

    const height = grid._0.items.len;
    const width = grid._0.items[0].items.len;
    const adjacent = [8]struct { x: i8, y: i8 }{
        .{ .x = -1, .y = -1 }, .{ .x = 0, .y = -1 },
        .{ .x = 1, .y = -1 },  .{ .x = -1, .y = 0 },
        .{ .x = 1, .y = 0 },   .{ .x = -1, .y = 1 },
        .{ .x = 0, .y = 1 },   .{ .x = 1, .y = 1 },
    };

    for (grid._0.items, 0..) |row, y| {
        for (row.items, 0..) |char, x| {
            if (char != roll) continue;

            var rolls: usize = 0;
            for (adjacent) |off| {
                const nbr: Point = .{
                    .x = switch (off.x) {
                        1 => if (x + 1 < width) x + 1 else continue,
                        -1 => if (x > 0) x - 1 else continue,
                        else => x,
                    },
                    .y = switch (off.y) {
                        1 => if (y + 1 < height) y + 1 else continue,
                        -1 => if (y > 0) y - 1 else continue,
                        else => y,
                    },
                };

                if (grid._0.items[nbr.y].items[nbr.x] == roll) {
                    rolls += 1;
                }
            }

            if (rolls < limit) {
                const pos: Point = .{ .x = x, .y = y };
                try list.append(alloc, pos);
            }
        }
    }

    return list;
}

/// The ASCII character representing a roll of paper.
const roll: u8 = '@';

/// An ASCII character representing any empty tile.
const empty: u8 = '.';

/// Below this number, rolls of paper are accessible.
const limit = 4;

/// A coordinate point `(x, y)`.
const Point = struct { x: usize, y: usize };

/// A rectangular grid of characters (`u8`s).
const Grid = struct {
    _0: std.ArrayListUnmanaged(std.ArrayListUnmanaged(u8)),

    const Self = @This();

    /// Read from given `text`. If successful, caller is given ownership of
    /// returned memory.  It is expected that the input will contain
    /// one-or-more lines, and that each line will have the same length.
    /// Any empty lines will be skipped.
    fn parse(alloc: std.mem.Allocator, text: []const u8) !Self {
        const Inner = std.ArrayListUnmanaged(std.ArrayListUnmanaged(u8));
        var self: Self = .{ ._0 = Inner.empty };
        errdefer self.deinit(alloc);

        var lines = std.mem.tokenizeScalar(u8, text, '\n');
        const first = lines.peek() orelse return error.NoInputLines;
        const width = first.len;
        if (width == 0) return error.EmptyInputLines;

        while (lines.next()) |line| {
            if (line.len != width) return error.UnevenLines;
            var list: std.ArrayListUnmanaged(u8) = .empty;
            try list.appendSlice(alloc, line);
            try self._0.append(alloc, list);
        }

        return self;
    }

    /// Free memory allocated for this grid.
    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        for (self._0.items) |*list| list.deinit(alloc);
        self._0.deinit(alloc);
    }
};

/// Our example input for day 4.
const example: []const u8 =
    \\..@@.@@@@.
    \\@@@.@.@.@@
    \\@@@@@.@.@@
    \\@.@@@@..@.
    \\@@.@@@@.@@
    \\.@@@@@@@.@
    \\.@.@.@.@@@
    \\@.@@@.@@@@
    \\.@@@@@@@@.
    \\@.@.@@@.@.
;
