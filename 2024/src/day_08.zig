//! Advent of Code 2024, Day 8: Resonant Collinearity.
const std = @import("std");
const utils = @import("utils.zig");
const Coord = utils.Coord;

/// Run both parts for day 8.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;
    const stdout = std.io.getStdOut().writer();
    const map = try utils.getInputFile(alloc, 8);
    defer alloc.free(map);

    const locations = try part1(alloc, map);
    try stdout.print("part 1: {d}\n", .{locations});
}

/// Day 8, part 1. Parse the map, grouping together antennae of the same
/// frequency. Then iterate over all pairs of antennae to map out all
/// antinodes, and count them up.
pub fn part1(alloc: std.mem.Allocator, text: []const u8) !u32 {
    var map = try Map.parse(alloc, text);
    defer map.deinit(alloc);
    return map.numAntinodes(alloc);
}

/// An (array) hashmap from `K` to a list of `V`s.
fn HashMapList(comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();

        in: std.AutoArrayHashMapUnmanaged(K, std.MultiArrayList(V)),

        /// Initialize as empty.
        const empty: Self = .{ .in = .empty };

        /// Append `value` to the list under `key`, initialising a new empty
        /// list if needed.
        fn appendAt(
            self: *Self,
            alloc: std.mem.Allocator,
            key: K,
            value: V,
        ) !void {
            const entry = try self.in.getOrPutValue(alloc, key, .empty);
            try entry.value_ptr.append(alloc, value);
        }

        /// Free the memory used by this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            for (self.in.entries.items(.value)) |*list| list.deinit(alloc);
            self.in.deinit(alloc);
        }
    };
}

/// A map - a collection of antennae within a grid.
const Map = struct {
    const Self = @This();

    antennae: HashMapList(u8, Coord(usize)),
    width: usize,
    height: usize,

    /// Count the number of antinodes within the bounds of the map.
    fn numAntinodes(self: Self, alloc: std.mem.Allocator) !u32 {
        var antinodes: std.AutoHashMapUnmanaged(Coord(isize), void) = .empty;
        defer antinodes.deinit(alloc);

        const bound_u: Coord(usize) = .{ .x = self.width, .y = self.height };
        const bound = bound_u.cast(isize) orelse return error.IntCast;

        for (self.antennae.in.entries.items(.value)) |list| {
            if (list.len < 2) continue;
            const xs: []usize = list.items(.x);
            const ys: []usize = list.items(.y);

            for (xs, ys, 1..) |one_x, one_y, i| {
                const one_u: Coord(usize) = .{ .x = one_x, .y = one_y };
                const one = one_u.cast(isize) orelse return error.IntCast;

                for (xs[i..], ys[i..]) |two_x, two_y| {
                    const two_u: Coord(usize) = .{ .x = two_x, .y = two_y };
                    const two = two_u.cast(isize) orelse return error.IntCast;

                    if (one.addBoundPos(one.sub(two), bound)) |node| {
                        try antinodes.put(alloc, node, {});
                    }
                    if (two.addBoundPos(two.sub(one), bound)) |node| {
                        try antinodes.put(alloc, node, {});
                    }
                }
            }
        }

        return antinodes.count();
    }

    /// Parse from a string. Any alphanumeric characters are recognised as
    /// antennae, any other characters are skipped over.
    fn parse(alloc: std.mem.Allocator, text: []const u8) !Self {
        var antennae: HashMapList(u8, Coord(usize)) = .empty;
        errdefer antennae.deinit(alloc);

        var lines = std.mem.tokenizeScalar(u8, text, '\n');
        var row: usize = 0;
        var width: ?usize = null;

        while (lines.next()) |line| : (row += 1) {
            if (width) |w| {
                if (line.len != w) return error.UnevenRows;
            } else {
                width = line.len;
            }

            for (line, 0..) |char, col| {
                if (std.ascii.isAlphanumeric(char)) {
                    const coord: Coord(usize) = .{ .x = col, .y = row };
                    try antennae.appendAt(alloc, char, coord);
                }
            }
        }

        return .{
            .antennae = antennae,
            .width = width orelse return error.ZeroRows,
            .height = row,
        };
    }

    /// Free the memory used by this type.
    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        self.antennae.deinit(alloc);
    }
};

test "Map.numAntinodes" {
    const alloc = std.testing.allocator;
    var map = try Map.parse(alloc, example_map);
    defer map.deinit(alloc);

    const count = try map.numAntinodes(alloc);
    try std.testing.expectEqual(14, count);
}

test "Map.parse" {
    const alloc = std.testing.allocator;
    const expectEqualSlices = std.testing.expectEqualSlices;
    const expectEqual = std.testing.expectEqual;

    // Expected map.
    var expected: Map = .{ .antennae = .empty, .height = 12, .width = 12 };
    defer expected.deinit(alloc);
    for ([_][]const u8{
        &.{ '0', 8, 1 }, &.{ '0', 5, 2 }, &.{ '0', 7, 3 },
        &.{ '0', 4, 4 }, &.{ 'A', 6, 5 }, &.{ 'A', 8, 8 },
        &.{ 'A', 9, 9 },
    }) |antenna| {
        const coord: Coord(usize) = .{ .x = antenna[1], .y = antenna[2] };
        try expected.antennae.appendAt(alloc, antenna[0], coord);
    }

    // Actual parsed map.
    var actual = try Map.parse(alloc, example_map);
    defer actual.deinit(alloc);

    // Compare for equality.
    try expectEqual(expected.width, actual.width);
    try expectEqual(expected.height, actual.height);
    try expectEqual(expected.antennae.in.count(), actual.antennae.in.count());
    for (expected.antennae.in.keys()) |exp_key| {
        const exp_list = expected.antennae.in.get(exp_key) orelse unreachable;
        const act_list_maybe = actual.antennae.in.get(exp_key);
        const act_list = act_list_maybe orelse return error.MissingKey;
        try expectEqualSlices(usize, exp_list.items(.x), act_list.items(.x));
        try expectEqualSlices(usize, exp_list.items(.y), act_list.items(.y));
    }
}

/// The running example of a map for day 8.
const example_map: []const u8 =
    \\............
    \\........0...
    \\.....0......
    \\.......0....
    \\....0.......
    \\......A.....
    \\............
    \\............
    \\........A...
    \\.........A..
    \\............
    \\............
;
