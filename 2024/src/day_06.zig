//! Advent of Code 2024, Day 6: Guard Gallivant.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 6.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;
    const stdout = std.io.getStdOut().writer();
    const map = try utils.getInputFile(alloc, 6);
    defer alloc.free(map);

    const count1 = try part1(alloc, map);
    try stdout.print("part 1: {d}\n", .{count1});
}

/// Day 6, part 1 - parse the map, set the guard on patrol and count the total
/// number of distinct positions they visit before leaving the map.
pub fn part1(alloc: std.mem.Allocator, text: []const u8) !u32 {
    var map = try Map(u32).parse(alloc, text);
    defer map.deinit(alloc);

    while (map.patrol(alloc)) {} else |err| switch (err) {
        error.GuardLeftMap => {},
        else => return err,
    }

    return map.visited.size;
}

test part1 {
    const alloc = std.testing.allocator;
    const visited = try part1(alloc, example_map);
    try std.testing.expectEqual(41, visited);
}

/// A coordinate - a position `(x, y)`.
fn Coord(comptime T: type) type {
    return struct {
        const Self = @This();

        x: T = 0,
        y: T = 0,

        /// Add two coordinates together. If non-null `bounds` are given,
        /// returns an error when the result would go beyond those bounds.
        fn add(self: Self, other: Self, bounds: ?Self) !Self {
            const new_x = self.x + other.x;
            const new_y = self.y + other.y;

            if (bounds) |bound| {
                if (new_x >= bound.x or new_y >= bound.y) {
                    return error.OutOfBounds;
                }
            }

            return .{ .x = new_x, .y = new_y };
        }

        /// Subtract one coordinate from another. Returns an error if the
        /// result would go into negative coordinates.
        fn sub(self: Self, other: Self) !Self {
            if (self.x < other.x or self.y < other.y) {
                return error.OutOfBounds;
            }

            return .{
                .x = self.x - other.x,
                .y = self.y - other.y,
            };
        }
    };
}

/// A wrapper around `std.math.order` at the given type `T`. Needed to satisfy
/// the type checker in some cases.
fn order(comptime T: type) fn (T, T) std.math.Order {
    return struct {
        fn inner(a: T, b: T) std.math.Order {
            return std.math.order(a, b);
        }
    }.inner;
}

/// A collection of obstacles. Stores two indices - two views into the
/// same data, for effecient query at cost of extra space usage.
///
/// The first is `.x_then_y` - a hash map from `x` coordinates into a sorted
/// list of `y` coordinates where there is an obstacle present. The second,
/// `.y_then_x`, does the same thing but with `y` and `x` swapped.
fn Obstacles(comptime T: type) type {
    return struct {
        const Self = @This();

        x_then_y: Index,
        y_then_x: Index,

        const Index =
            std.AutoArrayHashMapUnmanaged(T, std.ArrayListUnmanaged(T));
        const empty: Self =
            .{ .x_then_y = .empty, .y_then_x = .empty };

        /// Is the given coordinate `(x, y)` in this collection of obstacles?
        fn contains(self: Self, coord: Coord(T)) bool {
            const binarySearch = std.sort.binarySearch;
            const ys = self.x_then_y.get(coord.x) orelse return false;
            const index = binarySearch(T, ys.items, coord.y, order(T));
            return (index != null);
        }

        /// Add an obstacle to the collection. Not recommended for anything
        /// other than testing, as it is an inefficient way to update this data
        /// type.
        fn put(self: *Self, alloc: std.mem.Allocator, coord: Coord(T)) !void {
            const lowerBound = std.sort.lowerBound;

            const xs_entry =
                try self.y_then_x.getOrPutValue(alloc, coord.y, .empty);
            const xs_list = xs_entry.value_ptr.items;
            const insert_x_at = lowerBound(T, xs_list, coord.x, order(T));
            try xs_entry.value_ptr.insert(alloc, insert_x_at, coord.x);

            const ys_entry =
                try self.x_then_y.getOrPutValue(alloc, coord.x, .empty);
            const ys_list = ys_entry.value_ptr.items;
            const insert_y_at = lowerBound(T, ys_list, coord.y, order(T));
            try ys_entry.value_ptr.insert(alloc, insert_y_at, coord.y);
        }

        /// Deallocate all memory associated with this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            for (self.y_then_x.values()) |*xs_list| xs_list.deinit(alloc);
            for (self.x_then_y.values()) |*ys_list| ys_list.deinit(alloc);
            self.y_then_x.deinit(alloc);
            self.x_then_y.deinit(alloc);
        }
    };
}

/// A (cardinal) direction - North, East, South or West.
const Direction = enum {
    const Self = @This();

    North,
    East,
    South,
    West,

    /// Change directions as if we turned right by 90 degrees.
    fn turnRight(self: *Self) void {
        const self_index: usize = @intFromEnum(self.*);
        const count: usize = @typeInfo(Self).@"enum".fields.len;
        self.* = @enumFromInt(@mod(self_index + 1, count));
    }
};

/// The current state of a guard.
fn Guard(comptime T: type) type {
    return struct {
        pos: Coord(T),
        facing: Direction,
    };
}

/// A map - a rectangular grid, featuring some number of obstacles and one
/// guard located somewhere inside that grid.
fn Map(comptime T: type) type {
    return struct {
        const Self = @This();

        width: T,
        height: T,
        obstacles: Obstacles(T),
        visited: std.AutoHashMapUnmanaged(Coord(T), void),
        guard: Guard(T),

        /// Which (ASCII) character is used for obstacles.
        const obstacle: u8 = '#';

        /// Which (ASCII) characters are used for directions. These are
        /// expected to be distinct from each other.
        const directions: std.EnumArray(Direction, u8) =
            .init(.{ .North = '^', .East = '>', .South = 'v', .West = '<' });

        /// Set the guard off on patrol:
        ///
        /// * If there is an obstacle immediately in front of them, turn right
        ///   90 degrees and try patrolling again.
        /// * Otherwise, move them in a straight line in the direction they
        ///   are facing until they hit an obstacle.
        /// * If the guard would move past the edges of the map, stop them at
        ///   the edge just before leaving the map and return an error.
        ///
        /// Updates the `.visited` map with any new positions visited during
        /// this patrol.
        fn patrol(self: *Self, alloc: std.mem.Allocator) !void {
            const bounds: Coord(T) = .{ .x = self.width, .y = self.height };
            var loops: u8 = 0;

            // If there is an obstacle immediately in front of us, turn right
            // until there is no obstacle in front.
            while (true) : (loops += 1) {
                if (loops > 4) return error.SurroundedByObstacles;
                const in_front: Coord(T) = switch (self.guard.facing) {
                    .North => try self.guard.pos.sub(.{ .y = 1 }),
                    .East => try self.guard.pos.add(.{ .x = 1 }, bounds),
                    .South => try self.guard.pos.add(.{ .y = 1 }, bounds),
                    .West => try self.guard.pos.sub(.{ .x = 1 }),
                };
                if (!self.obstacles.contains(in_front)) break;
                self.guard.facing.turnRight();
            }

            // Now there is no obstacle immediately in front of us, so we can
            // make some progress. Move as far forward as possible without
            // hitting any obstacles, throwing an error if we would move past
            // the edges of the map.
            const lowerBound = std.sort.lowerBound;
            const pos_x = self.guard.pos.x;
            const pos_y = self.guard.pos.y;

            // Suppose we are moving north/south: then start by looking up the
            // obstacles in our current column (our `x` coordinate). Find the
            // first one we would collide with (if any). Move our position to
            // *just* in-front of that obstacle.
            //
            // If there are no obstacles inbetween us and the edge of the map,
            // then we would leave the map completely! In that case, move us
            // right up to the edge as far as we can go, and return an error
            // to signal this scenario.
            //
            // The logic is exactly the same for east/west, just with the roles
            // of `x` and `y` swapped.
            switch (self.guard.facing) {
                .North, .South => |facing| {
                    const maybe_ys = self.obstacles.x_then_y.get(pos_x);
                    const obs_ys = if (maybe_ys) |ys| ys.items else &.{};
                    const below = lowerBound(T, obs_ys, pos_y, order(T));
                    const none_above = (below == 0);
                    const none_below = (below == obs_ys.len);

                    const edge = self.height - 1;
                    self.guard.pos.y = switch (facing) {
                        .North => if (none_above) 0 else obs_ys[below - 1] + 1,
                        .South => if (none_below) edge else obs_ys[below] - 1,
                        else => unreachable,
                    };

                    const up = (facing == .North);
                    const start = if (up) self.guard.pos.y else pos_y;
                    const stop = if (up) pos_y else self.guard.pos.y;
                    var y: T = start;
                    while (y <= stop) : (y += 1) {
                        const visited: Coord(T) = .{ .x = pos_x, .y = y };
                        try self.visited.put(alloc, visited, {});
                    }

                    const would_leave_map =
                        (facing == .North and none_above) or
                        (facing == .South and none_below);
                    if (would_leave_map) return error.GuardLeftMap;
                },

                .East, .West => |facing| {
                    const maybe_xs = self.obstacles.y_then_x.get(pos_y);
                    const obs_xs = if (maybe_xs) |xs| xs.items else &.{};
                    const after = lowerBound(T, obs_xs, pos_x, order(T));
                    const none_before = (after == 0);
                    const none_after = (after == obs_xs.len);

                    const edge = self.width - 1;
                    self.guard.pos.x = switch (facing) {
                        .East => if (none_after) edge else obs_xs[after] - 1,
                        .West => if (none_before) 0 else obs_xs[after - 1] + 1,
                        else => unreachable,
                    };

                    const right = (facing == .East);
                    const start = if (right) pos_x else self.guard.pos.x;
                    const stop = if (right) self.guard.pos.x else pos_x;
                    var x: T = start;
                    while (x <= stop) : (x += 1) {
                        const visited: Coord(T) = .{ .x = x, .y = pos_y };
                        try self.visited.put(alloc, visited, {});
                    }

                    const would_leave_map =
                        (facing == .East and none_after) or
                        (facing == .West and none_before);
                    if (would_leave_map) return error.GuardLeftMap;
                },
            }
        }

        /// Parse a map from a string. It is expected that the map will be a
        /// grid (every row has the same length), and there should be exactly
        /// one guard on the map.
        fn parse(alloc: std.mem.Allocator, text: []const u8) !Self {
            var lines = std.mem.tokenizeScalar(u8, text, '\n');
            var width: ?T = null;
            var height: T = 0;
            var guard: ?Guard(T) = null;
            const cast = std.math.cast;

            // To populate this type effeciently, we fill the lists in whatever
            // order obstacles come in, and only sort them once they are full.
            // The lists being temporarily unsorted does violate the expected
            // invariant on this type, so it is important to sort them before
            // returning.
            var obstacles: Obstacles(T) = .empty;
            errdefer obstacles.deinit(alloc);

            // Loop over each line in the input.
            while (lines.next()) |line| : (height += 1) {
                const len: T =
                    cast(T, line.len) orelse return error.IntCastError;
                if (width) |w| {
                    if (len != w) return error.UnevenLineLengths;
                } else {
                    width = len;
                }

                // Loop over each character in this line.
                for (line, 0..) |char, x| {
                    const coord: Coord(T) = .{
                        .x = cast(T, x) orelse return error.IntCastError,
                        .y = height,
                    };

                    // If it's an obstacle, then insert it into `obstacles`.
                    if (char == obstacle) {
                        const xs_entry = try obstacles
                            .y_then_x
                            .getOrPutValue(alloc, coord.y, .empty);
                        try xs_entry.value_ptr.append(alloc, coord.x);

                        const ys_entry = try obstacles
                            .x_then_y
                            .getOrPutValue(alloc, coord.x, .empty);
                        try ys_entry.value_ptr.append(alloc, coord.y);

                        continue;
                    }

                    // If it's a guard facing in some direction, make a record
                    // of that. Check that there is not more than one guard.
                    inline for (@typeInfo(Direction).@"enum".fields) |field| {
                        const dir: Direction = @enumFromInt(field.value);
                        if (char == directions.get(dir)) {
                            if (guard != null) return error.MultipleGuards;
                            guard = .{ .pos = coord, .facing = dir };
                        }
                    }

                    // If control flow would reach here, then it is not a
                    // recognised character that has any meaning to us. So we
                    // simply skip over it.
                }
            }

            // We should have a width to report and have encountered a guard.
            const some_width = width orelse return error.ZeroWidth;
            const some_guard = guard orelse return error.MissingGuard;

            // If we got this far, then we are definitely returning from this
            // function. So now we will do the work of sorting these lists.
            for (obstacles.y_then_x.values()) |x_coords| {
                std.sort.pdq(T, x_coords.items, {}, std.sort.asc(T));
            }
            for (obstacles.x_then_y.values()) |y_coords| {
                std.sort.pdq(T, y_coords.items, {}, std.sort.asc(T));
            }

            return .{
                .width = some_width,
                .height = height,
                .obstacles = obstacles,
                .guard = some_guard,
                .visited = .empty,
            };
        }

        /// Free the backing memory used by this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.obstacles.deinit(alloc);
            self.visited.deinit(alloc);
        }
    };
}

test "Map.parse" {
    const alloc = std.testing.allocator;
    const Int = u32;

    // Populate the map we expect.
    var expected_map: Map(Int) = .{
        .height = 10,
        .width = 10,
        .guard = .{
            .facing = .North,
            .pos = .{ .x = 4, .y = 6 },
        },
        .obstacles = .empty,
        .visited = .empty,
    };
    defer expected_map.deinit(alloc);

    const coords = [_]Coord(Int){
        .{ .y = 0, .x = 4 }, .{ .y = 1, .x = 9 },
        .{ .y = 3, .x = 2 }, .{ .y = 4, .x = 7 },
        .{ .y = 6, .x = 1 }, .{ .y = 7, .x = 8 },
        .{ .y = 8, .x = 0 }, .{ .y = 9, .x = 6 },
    };
    for (coords) |coord| {
        try expected_map.obstacles.put(alloc, coord);
    }

    // Parse the actual map from a string.
    var actual_map = try Map(Int).parse(alloc, example_map);
    defer actual_map.deinit(alloc);

    // Compare the two for equality.
    try std.testing.expectEqual(expected_map.width, actual_map.width);
    try std.testing.expectEqual(expected_map.height, actual_map.height);
    try std.testing.expectEqual(expected_map.guard, actual_map.guard);

    try std.testing.expectEqual(
        expected_map.visited.size,
        actual_map.visited.size,
    );
    var expected_visited = expected_map.visited.keyIterator();
    var actual_visited = actual_map.visited.keyIterator();
    while (expected_visited.next()) |expected| {
        const actual = actual_visited.next() orelse return error.MissingKey;
        try std.testing.expectEqual(expected.*, actual.*);
    }
    try std.testing.expectEqual(null, actual_visited.next());

    const fields = [_][]const u8{ "x_then_y", "y_then_x" };
    inline for (fields) |field| {
        const expected = @field(expected_map.obstacles, field);
        const actual = @field(actual_map.obstacles, field);
        try std.testing.expectEqual(expected.entries.len, actual.entries.len);

        for (expected.keys()) |key| {
            try std.testing.expect(actual.contains(key));
            const exp = expected.get(key) orelse return error.MissingKey;
            const act = actual.get(key) orelse return error.MissingKey;
            try std.testing.expectEqualSlices(Int, exp.items, act.items);
        }
    }
}

/// Our running example of a map for day 6.
const example_map: []const u8 =
    \\....#.....
    \\.........#
    \\..........
    \\..#.......
    \\.......#..
    \\..........
    \\.#..^.....
    \\........#.
    \\#.........
    \\......#...
;
