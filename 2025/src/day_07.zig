//! Advent of Code 2025, Day 7: Laboratories.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Alloc = utils.Alloc;
const Input = utils.Input;

/// Run both parts for day 7.
pub fn main() !void {
    try runDay(
        .{ .day = 7 },
        .{ part1, Alloc, Input },
        .{ part2, Alloc, Input },
    );
}

/// Tachyon beam start.
const start_char: u8 = 'S';

/// Tachyon beam splitter.
const splitter_char: u8 = '^';

/// Day 7, part 1 - how many times will the beam be split?
pub fn part1(alloc: std.mem.Allocator, input: []const u8) !u64 {
    var beams: std.AutoArrayHashMapUnmanaged(usize, void) = .empty;
    defer beams.deinit(alloc);

    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    const first = lines.next() orelse return error.NoInput;
    const maybe = std.mem.indexOfScalar(u8, first, start_char);
    const start = maybe orelse return error.NoStart;
    try beams.put(alloc, start, {});

    var count: u64 = 0;
    var splits: std.ArrayList(usize) = .empty;
    defer splits.deinit(alloc);

    while (lines.next()) |line| {
        splits.clearRetainingCapacity();

        for (beams.keys()) |beam| {
            if (line[beam] == splitter_char) {
                if (beam == 0) return error.OverflowLeft;
                if (beam == line.len - 1) return error.OverflowRight;
                try splits.append(alloc, beam);
                count += 1;
            }
        }

        for (splits.items) |split| {
            const removed = beams.swapRemove(split);
            std.debug.assert(removed);
            try beams.put(alloc, split - 1, {});
            try beams.put(alloc, split + 1, {});
        }
    }

    return count;
}

test part1 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(21, part1(alloc, example));
}

/// Day 7, part 2 - in total, how many different timelines would a single
/// tachyon particle end up on?
pub fn part2(alloc: std.mem.Allocator, input: []const u8) !u64 {
    var beams: std.AutoArrayHashMapUnmanaged(usize, u64) = .empty;
    defer beams.deinit(alloc);

    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    const first = lines.next() orelse return error.NoInput;
    const maybe = std.mem.indexOfScalar(u8, first, start_char);
    const start = maybe orelse return error.NoStart;
    try beams.put(alloc, start, 1);

    var splits: std.ArrayList(usize) = .empty;
    defer splits.deinit(alloc);

    while (lines.next()) |line| {
        splits.clearRetainingCapacity();

        for (beams.keys()) |pos| {
            if (line[pos] == splitter_char) {
                if (pos == 0) return error.OverflowLeft;
                if (pos == line.len - 1) return error.OverflowRight;
                try splits.append(alloc, pos);
            }
        }

        for (splits.items) |split| {
            const beam = beams.fetchSwapRemove(split).?;
            for ([_]usize{ split - 1, split + 1 }) |pos| {
                const entry = try beams.getOrPutValue(alloc, pos, 0);
                entry.value_ptr.* += beam.value;
            }
        }
    }

    var timelines: u64 = 0;
    for (beams.values()) |beam| {
        timelines += beam;
    }

    return timelines;
}

test part2 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(40, part2(alloc, example));
}

/// Our example input for day 7.
const example: []const u8 =
    \\.......S.......
    \\...............
    \\.......^.......
    \\...............
    \\......^.^......
    \\...............
    \\.....^.^.^.....
    \\...............
    \\....^.^...^....
    \\...............
    \\...^.^...^.^...
    \\...............
    \\..^...^.....^..
    \\...............
    \\.^.^.^.^.^...^.
    \\...............
;
