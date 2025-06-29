//! Advent of Code 2024, Day 6: Guard Gallivant.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 6.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;
    const stdout = std.io.getStdOut().writer();
    const map = try utils.getInputFile(alloc, 6);
    defer alloc.free(map);

    const visited = try part1(alloc, map);
    try stdout.print("part 1: {d}\n", .{visited});

    const loops = try part2(alloc, map);
    try stdout.print("part 2: {d}\n", .{loops});
}

/// Day 6, part 1 - parse the map, set the guard on patrol and count the total
/// number of distinct positions they visit before leaving the map.
pub fn part1(alloc: std.mem.Allocator, text: []const u8) !u32 {
    var map = try Map(u32).parse(alloc, text);
    defer map.deinit(alloc);
    var path = try map.completePatrol(alloc);
    defer path.deinit(alloc);

    var seen: std.AutoHashMapUnmanaged(Coord(u32), void) = .empty;
    defer seen.deinit(alloc);
    for (path.items) |step| {
        try seen.put(alloc, step.pos, {});
    }

    return std.math.cast(u32, seen.size) orelse error.IntCastError;
}

test part1 {
    const alloc = std.testing.allocator;
    const visited = try part1(alloc, example_map);
    try std.testing.expectEqual(41, visited);
}

/// Day 6, part 2 - parse the map, and analyse the guards patrol route: how
/// many different positions could a single new obstacle be placed at, in
/// order to turn that patrol route into a closed loop?
pub fn part2(alloc: std.mem.Allocator, text: []const u8) !u32 {
    var map = try Map(u32).parse(alloc, text);
    defer map.deinit(alloc);
    var loops = try map.obstacleLoops(alloc);
    defer loops.deinit(alloc);

    return std.math.cast(u32, loops.count()) orelse error.IntCastError;
}

test part2 {
    const alloc = std.testing.allocator;
    const loops = try part2(alloc, example_map);
    try std.testing.expectEqual(6, loops);
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

/// A mode for the `find` function to search by.
const Target = enum { Pred, Succ };

/// Performs a binary search to find the closest element to `key` in the given
/// treap. Depending on the choice of target:
///
/// * `.Pred` means, find the predecessor of `key` - the largest element
///   smaller than it.
/// * `.Succ` means, find the successor instead - the least element
///   bigger than `key`.
fn find(
    comptime K: type,
    treap: std.Treap(K, order(K)),
    key: K,
    target: Target,
) ?*std.Treap(K, order(K)).Node {
    const Node = std.Treap(K, order(K)).Node;
    var current: ?*Node = treap.root;
    var candidate: ?*Node = null;

    while (current) |node| {
        const comp = order(K)(key, node.key);
        if (target == .Pred and comp == .gt) candidate = node;
        if (target == .Succ and comp == .lt) candidate = node;
        current = switch (comp) {
            .lt => node.children[0],
            .eq => node.children[if (target == .Pred) 0 else 1],
            .gt => node.children[1],
        };
    }

    return candidate;
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
        const Treap = std.Treap(T, order(T));

        x_then_y: std.AutoArrayHashMapUnmanaged(T, Treap),
        y_then_x: std.AutoArrayHashMapUnmanaged(T, Treap),
        pool: ?std.heap.MemoryPool(Treap.Node),

        /// An empty set of obstacles.
        const empty: Self =
            .{ .x_then_y = .empty, .y_then_x = .empty, .pool = null };

        /// Is the given coordinate `(x, y)` in this collection of obstacles?
        fn contains(self: Self, coord: Coord(T)) bool {
            var ys = self.x_then_y.get(coord.x) orelse return false;
            return ys.getEntryFor(coord.y).node != null;
        }

        /// Add an obstacle to the collection.
        fn put(self: *Self, alloc: std.mem.Allocator, coord: Coord(T)) !void {
            const pool: *std.heap.MemoryPool(Treap.Node) =
                if (self.pool) |*p| p else init: {
                    self.pool = .init(alloc);
                    break :init &self.pool.?;
                };

            const xs = try self.y_then_x.getOrPutValue(alloc, coord.y, .{});
            var x_entry = xs.value_ptr.getEntryFor(coord.x);
            if (x_entry.node == null) x_entry.set(try pool.create());

            const ys = try self.x_then_y.getOrPutValue(alloc, coord.x, .{});
            var y_entry = ys.value_ptr.getEntryFor(coord.y);
            if (y_entry.node == null) y_entry.set(try pool.create());
        }

        /// Remove an obstacle from the collection.
        fn remove(self: *Self, coord: Coord(T)) void {
            if (self.y_then_x.getPtr(coord.y)) |xs| {
                var x_entry = xs.getEntryFor(coord.x);
                x_entry.set(null);
            }
            if (self.x_then_y.getPtr(coord.x)) |ys| {
                var y_entry = ys.getEntryFor(coord.y);
                y_entry.set(null);
            }
        }

        /// Deallocate all memory associated with this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.x_then_y.deinit(alloc);
            self.y_then_x.deinit(alloc);
            if (self.pool) |*pool| pool.deinit();
        }
    };
}

/// The status of a patrolling guard.
const Status = enum { OnPatrol, LeftMap };

/// A (cardinal) direction - North, East, South or West.
const Direction = enum {
    const Self = @This();

    North,
    East,
    South,
    West,

    /// Change directions as if we turned right by 90 degrees.
    fn turnRight(self: *Self) void {
        const index: usize = @intFromEnum(self.*);
        const count: usize = @typeInfo(Self).@"enum".fields.len;
        self.* = @enumFromInt(@mod(index + 1, count));
    }
};

/// A position and a direction.
fn Step(comptime T: type) type {
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
        guard: Step(T),

        /// Which (ASCII) character is used for obstacles.
        const obstacle: u8 = '#';

        /// Which (ASCII) characters are used for directions. These are
        /// expected to be distinct from each other.
        const directions: std.EnumArray(Direction, u8) =
            .init(.{ .North = '^', .East = '>', .South = 'v', .West = '<' });

        /// Return every possible location an obstacle could be placed at,
        /// which would result in the guard's patrol becoming a closed loop.
        fn obstacleLoops(
            self: *Self,
            alloc: std.mem.Allocator,
        ) !std.AutoHashMapUnmanaged(Coord(T), void) {
            // Remember the guard's initial location.
            const initial = self.guard;

            // The possible locations an obstacle *could* be placed that might
            // influence the result of a patrol.
            var candidates = try self.completePatrol(alloc);
            defer candidates.deinit(alloc);

            // A reusable buffer for remembering our footsteps on patrol.
            var footsteps: std.AutoHashMapUnmanaged(Step(T), void) = .empty;
            defer footsteps.deinit(alloc);

            // The locations at which placing an obstacle *would* influence
            // a patrol by turning it into a closed loop. This will be our
            // return value.
            var loops: std.AutoHashMapUnmanaged(Coord(T), void) = .empty;
            errdefer loops.deinit(alloc);

            // Iterate over the candidate obstacle locations.
            for (candidates.items[1..]) |block| {
                // The guards initial position is not allowed, as they
                // are currently occupying that spot.
                if (std.meta.eql(block.pos, initial.pos)) continue;

                // Prepare a patrol route with this obstacle in place.
                try self.obstacles.put(alloc, block.pos);
                defer self.obstacles.remove(block.pos);
                self.guard = initial;
                footsteps.clearRetainingCapacity();

                // Simulate the patrol to determine its outcome - whether it
                // is a loop or not.
                const is_loop: bool =
                    while (self.patrol(alloc, null)) |status| {
                        if (status == .LeftMap) break false;
                        if (footsteps.contains(self.guard)) break true;
                        try footsteps.put(alloc, self.guard, {});
                    } else |err| return err;

                // If this patrol results in a loop, add it to the list.
                if (is_loop) try loops.put(alloc, block.pos, {});
            }

            return loops;
        }

        /// Complete the guards patrol, returning the full route walked.
        fn completePatrol(
            self: *Self,
            alloc: std.mem.Allocator,
        ) !std.ArrayListUnmanaged(Step(T)) {
            var path: std.ArrayListUnmanaged(Step(T)) = .empty;
            errdefer path.deinit(alloc);

            while (self.patrol(alloc, &path)) |status| {
                switch (status) {
                    .OnPatrol => {},
                    .LeftMap => break,
                }
            } else |err| return err;

            try path.append(alloc, self.guard);
            return path;
        }

        /// Set the guard off on patrol:
        ///
        /// * If there is an obstacle immediately in front of them, turn right
        ///   90 degrees and try patrolling again.
        /// * Otherwise, move them in a straight line in the direction they
        ///   are facing until they hit an obstacle or reach the edge of
        ///   the map.
        ///
        /// Returns the guards final status, and populates `path` (if given)
        /// with every position visited during this patrol, excluding the final
        /// position they now occupy.
        fn patrol(
            self: *Self,
            alloc: std.mem.Allocator,
            path: ?*std.ArrayListUnmanaged(Step(T)),
        ) !Status {
            // If there is an obstacle immediately in front of us, turn right
            // until there is no longer any obstacle in front.
            try self.readyGuard();

            // Now there is no obstacle immediately in front of us, so we can
            // make some progress. Move as far forward as possible without
            // hitting any obstacles, throwing an error if we would move past
            // the edges of the map.
            return switch (self.guard.facing) {
                .North => try self.walkVert(alloc, true, path),
                .East => try self.walkHoriz(alloc, false, path),
                .South => try self.walkVert(alloc, false, path),
                .West => try self.walkHoriz(alloc, true, path),
            };
        }

        /// Checks for an obstacle immediately in front of `self.guard`. If
        /// we find one, then have the guard turn right 90 degrees and check
        /// again. Repeat this process until we have a free tile in front of
        /// us to start walking into.
        fn readyGuard(self: *Self) !void {
            const bounds: Coord(T) = .{ .x = self.width, .y = self.height };
            const tries = @typeInfo(Direction).@"enum".fields.len;

            for (0..tries) |_| {
                const in_front: Coord(T) = switch (self.guard.facing) {
                    .North => try self.guard.pos.sub(.{ .y = 1 }),
                    .East => try self.guard.pos.add(.{ .x = 1 }, bounds),
                    .South => try self.guard.pos.add(.{ .y = 1 }, bounds),
                    .West => try self.guard.pos.sub(.{ .x = 1 }),
                };
                switch (self.obstacles.contains(in_front)) {
                    false => return,
                    true => self.guard.facing.turnRight(),
                }
            }

            return error.SurroundedByObstacles;
        }

        /// Walk the guard horizontally, north or south depending on whether
        /// `up == true`. Start by looking up the obstacles in our current
        /// column (our `x` coordinate). Find the first one we would collide
        /// with (if any). Move our position to *just* in-front of that
        /// obstacle.
        ///
        /// If there are no obstacles inbetween us and the edge of the map,
        /// move right up to the edge as far as we can go.
        fn walkVert(
            self: *Self,
            alloc: std.mem.Allocator,
            comptime up: bool,
            path: ?*std.ArrayListUnmanaged(Step(T)),
        ) !Status {
            // Remember initial position.
            const pos_x = self.guard.pos.x;
            const pos_y = self.guard.pos.y;

            // Get the first obstacle we will hit (if any).
            const maybe = self.obstacles.x_then_y.get(pos_x);
            const above = if (maybe) |ys| find(T, ys, pos_y, .Pred) else null;
            const below = if (maybe) |ys| find(T, ys, pos_y, .Succ) else null;

            // Move the guards position.
            self.guard.pos.y = switch (up) {
                true => if (above) |a| (a.key + 1) else 0,
                false => if (below) |b| (b.key - 1) else self.height - 1,
            };

            // Register every position we walked through in `path`.
            if (path) |list| {
                var y: T = pos_y;
                while (y != self.guard.pos.y) {
                    const dir: Direction = if (up) .North else .South;
                    const coord: Coord(T) = .{ .x = pos_x, .y = y };
                    try list.append(alloc, .{ .pos = coord, .facing = dir });
                    if (up) y -= 1 else y += 1;
                }
            }

            // Check for the edge.
            const would_leave_map =
                (up and above == null) or
                (!up and below == null);
            return if (would_leave_map) .LeftMap else .OnPatrol;
        }

        /// Walk the guard vertically, east or west depending on whether
        /// `left == true`. See `walkVert` for comments, as the logic here is
        /// the same just with the `x` and `y` axes swapped.
        fn walkHoriz(
            self: *Self,
            alloc: std.mem.Allocator,
            comptime left: bool,
            path: ?*std.ArrayListUnmanaged(Step(T)),
        ) !Status {
            // Remember initial position.
            const pos_x = self.guard.pos.x;
            const pos_y = self.guard.pos.y;

            // Get the first obstacle we will hit (if any).
            const maybe = self.obstacles.y_then_x.get(pos_y);
            const before = if (maybe) |xs| find(T, xs, pos_x, .Pred) else null;
            const after = if (maybe) |xs| find(T, xs, pos_x, .Succ) else null;

            // Move the guards position.
            self.guard.pos.x = switch (left) {
                true => if (before) |b| (b.key + 1) else 0,
                false => if (after) |a| (a.key - 1) else self.width - 1,
            };

            // Register every position we walked through in `path`.
            if (path) |list| {
                var x: T = pos_x;
                while (x != self.guard.pos.x) {
                    const dir: Direction = if (left) .West else .East;
                    const coord: Coord(T) = .{ .x = x, .y = pos_y };
                    try list.append(alloc, .{ .pos = coord, .facing = dir });
                    if (left) x -= 1 else x += 1;
                }
            }

            // Check for the edge.
            const would_leave_map =
                (left and before == null) or
                (!left and after == null);
            return if (would_leave_map) .LeftMap else .OnPatrol;
        }

        /// Parse a map from a string. It is expected that the map will be a
        /// grid (every row has the same length), and there should be exactly
        /// one guard on the map.
        fn parse(alloc: std.mem.Allocator, text: []const u8) !Self {
            var lines = std.mem.tokenizeScalar(u8, text, '\n');
            var width: ?T = null;
            var height: T = 0;
            var guard: ?Step(T) = null;
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
                        try obstacles.put(alloc, coord);
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

            return .{
                .width = some_width,
                .height = height,
                .obstacles = obstacles,
                .guard = some_guard,
            };
        }

        /// Free the backing memory used by this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.obstacles.deinit(alloc);
        }
    };
}

test "Map.obstacleLoops" {
    const alloc = std.testing.allocator;
    const Int = u32;
    const expected = [_]Coord(Int){
        .{ .x = 3, .y = 6 }, .{ .x = 6, .y = 7 },
        .{ .x = 7, .y = 7 }, .{ .x = 1, .y = 8 },
        .{ .x = 3, .y = 8 }, .{ .x = 7, .y = 9 },
    };

    var map = try Map(Int).parse(alloc, example_map);
    defer map.deinit(alloc);
    var loops = try map.obstacleLoops(alloc);
    defer loops.deinit(alloc);

    try std.testing.expectEqual(expected.len, loops.count());
    for (expected) |coord| {
        try std.testing.expect(loops.contains(coord));
    }
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

    const fields = [_][]const u8{ "x_then_y", "y_then_x" };
    inline for (fields) |field| {
        const Treap = std.Treap(Int, order(Int));
        const Index = std.AutoArrayHashMapUnmanaged(Int, Treap);
        const expected: Index = @field(expected_map.obstacles, field);
        const actual: Index = @field(actual_map.obstacles, field);
        try std.testing.expectEqual(expected.count(), actual.count());

        for (expected.keys()) |key| {
            try std.testing.expect(actual.contains(key));
            var exp_treap = expected.get(key) orelse return error.MissingKey;
            var act_treap = actual.get(key) orelse return error.MissingKey;
            var exp_iter = exp_treap.inorderIterator();
            var act_iter = act_treap.inorderIterator();
            while (exp_iter.next()) |exp| {
                const act = act_iter.next() orelse return error.MissingObs;
                try std.testing.expectEqual(exp.key, act.key);
            }
            try std.testing.expectEqual(null, act_iter.next());
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
