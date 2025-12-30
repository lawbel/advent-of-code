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
        .{ part2, Input },
    );
}

/// Day 4, part 1 - how many rolls of paper can be accessed by a forklift?
pub fn part1(alloc: std.mem.Allocator, input: []const u8) !u32 {
    var accessible: u32 = 0;
    var grid: Grid = try .parse(alloc, input);
    defer grid.deinit(alloc);

    const roll = '@';
    const limit = 4;
    const height = grid._0.items.len;
    const width = grid._0.items[0].items.len;
    const adjacent = [8]struct { x: isize, y: isize }{
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
                const nbr = .{
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

            if (rolls < limit) accessible += 1;
        }
    }

    return accessible;
}

test part1 {
    const alloc = std.testing.allocator;
    const accessible = try part1(alloc, example);
    try std.testing.expectEqual(13, accessible);
}

/// Day 4, part 2 - [...]
pub fn part2(input: []const u8) !u64 {
    _ = input;
    return 0;
}

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
