//! Advent of Code 2025, Day 8: Playground.
const std = @import("std");
const utils = @import("utils.zig");
const runDay = utils.runDay;
const Alloc = utils.Alloc;
const Input = utils.Input;

/// Run both parts for day 8.
pub fn main() !void {
    try runDay(
        .{ .day = 8 },
        .{ part1, Alloc, Input, 1000 },
        .{ part2, Alloc, Input },
    );
}

/// Day 8, part 1 - what do you get if you multiply together the sizes of the
/// three largest circuits?
pub fn part1(alloc: std.mem.Allocator, input: []const u8, limit: usize) !u64 {
    const T: type = i64;
    const top: usize = 3;

    var points = try Point(T).parseMany(alloc, input);
    defer points.deinit(alloc);

    var circuits: Circuits(T) = .empty;
    var pairs = try closestPairs(alloc, T, points.items, limit);
    defer circuits.deinit(alloc);
    defer pairs.deinit();

    try circuits.addPoints(alloc, points.items);
    try circuits.connect(alloc, pairs.items);
    return circuits.multiplyLargest(alloc, top);
}

test part1 {
    const alloc = std.testing.allocator;
    try std.testing.expectEqual(40, part1(alloc, example, 10));
}

/// Day 8, part 2 - [...]
pub fn part2(alloc: std.mem.Allocator, input: []const u8) !u64 {
    _ = alloc;
    _ = input;

    return 0;
}

/// Given a collection of points, which `n` pairs are closest together?
fn closestPairs(
    alloc: std.mem.Allocator,
    comptime T: type,
    points: []const Point(T),
    n: usize,
) !std.PriorityQueue(Pair(T), void, Pair(T).compare) {
    const Queue = std.PriorityQueue(Pair(T), void, Pair(T).compare);
    var closest: Queue = .init(alloc, {});
    errdefer closest.deinit();

    var max: T = 0;
    for (points, 0..) |one, i| {
        if (i + 1 >= points.len) continue;
        for (points[i + 1 ..]) |two| {
            const pair: Pair(T) = .{ .l = one, .r = two };
            const quad: T = pair.l.quadrance(pair.r);

            if (closest.count() < n) {
                try closest.add(pair);
                max = @max(max, quad);
            } else if (quad < max) {
                _ = closest.remove();
                try closest.add(pair);
                const new = closest.peek().?;
                max = new.l.quadrance(new.r);
            }
        }
    }

    return closest;
}

test closestPairs {
    const alloc = std.testing.allocator;
    const T: type = i16;
    const points = [_]Point(T){
        .from(.{ 0, 0, 0 }),
        .from(.{ 1, 1, 1 }),
        .from(.{ 3, 3, 3 }),
        .from(.{ 7, 7, 7 }),
        .from(.{ 15, 15, 15 }),
        .from(.{ 33, 33, 33 }),
    };

    for ([_]usize{ 1, 2, 3, 5, 10 }) |n| {
        var expected: std.ArrayList(Pair(T)) = .empty;
        defer expected.deinit(alloc);
        for (points, 0..) |one, i| {
            if (i + 1 >= points.len) continue;
            for (points[i + 1 ..]) |two| {
                try expected.append(alloc, .{ .l = one, .r = two });
            }
        }
        std.sort.insertion(Pair(T), expected.items, {}, Pair(T).lessThan);

        var pairs = try closestPairs(alloc, T, &points, n);
        var actual: std.ArrayList(Pair(T)) = .empty;
        defer pairs.deinit();
        defer actual.deinit(alloc);

        while (pairs.removeOrNull()) |pair| try actual.append(alloc, pair);
        std.mem.reverse(Pair(T), actual.items);

        try std.testing.expectEqualSlices(
            Pair(T),
            expected.items[0..n],
            actual.items,
        );
    }
}

/// A collection of points, organised into disjoint sets.
fn Circuits(comptime T: type) type {
    return struct {
        connections: DisjointSet(Point(T)),

        const Self = @This();
        const empty: Self = .{ .connections = .empty };

        /// Free backing memory for this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.connections.deinit(alloc);
        }

        /// Add each given point to the collection, as it's own isolated
        /// circuited not connected to anything else.
        fn addPoints(
            self: *Self,
            alloc: std.mem.Allocator,
            points: []const Point(T),
        ) !void {
            for (points) |point| {
                _ = try self.connections.makeSet(alloc, point);
            }
        }

        /// Connect together each given pair of points into the same circuit.
        fn connect(
            self: *Self,
            alloc: std.mem.Allocator,
            pairs: []const Pair(T),
        ) !void {
            for (pairs) |pair| {
                _ = try self.connections.merge(alloc, pair.l, pair.r);
            }
        }

        /// Fetch the `n` largest circuits in the collection, and multiply
        /// together their sizes.
        fn multiplyLargest(
            self: *Self,
            alloc: std.mem.Allocator,
            n: usize,
        ) !usize {
            var roots = try self.connections.rootIds(alloc);
            defer roots.deinit(alloc);

            const greaterThan = DisjointSet(Point(T)).greaterThan;
            std.sort.heap(usize, roots.items, self.connections, greaterThan);

            var total: u64 = 1;
            for (roots.items[0..n]) |set| {
                total *= self.connections.nodes.items[set].size;
            }

            return total;
        }
    };
}

/// A node in a `DisjointSet`.
const Node = struct {
    /// The index / ID of the parent node. Root nodes point to themselves.
    parent: usize,
    /// The size of the set this node is part of. It is not recommended to
    /// access this field for anything other than root nodes, as the value
    /// may be nonsensical.
    size: usize,
};

/// A collection of values of type `T`, organised into disjoint sets.
/// The given type needs to be auto-hashable as we internally use a hash map.
fn DisjointSet(comptime T: type) type {
    return struct {
        ids: std.AutoArrayHashMapUnmanaged(T, usize),
        nodes: std.ArrayList(Node),

        const Self = @This();
        const empty: Self = .{ .ids = .empty, .nodes = .empty };

        /// Free backing memory.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.ids.deinit(alloc);
            self.nodes.deinit(alloc);
        }

        /// Make a new singleton set `{ value }` and add it to the collection.
        /// Returns the ID assigned to it.
        fn makeSet(self: *Self, alloc: std.mem.Allocator, value: T) !usize {
            const id = self.nodes.items.len;
            const entry: Node = .{ .parent = id, .size = 1 };
            try self.ids.put(alloc, value, id);
            try self.nodes.append(alloc, entry);
            return id;
        }

        /// Get the ID of `value`.
        fn getId(self: *Self, value: T) ?usize {
            return self.ids.get(value);
        }

        /// Get the ID of `value`; if it doesn't have one, then add it as its
        /// own singleton with `makeSet` and return its new ID.
        fn getMakeId(self: *Self, alloc: std.mem.Allocator, value: T) !usize {
            return self.getId(value) orelse self.makeSet(alloc, value);
        }

        /// Find the root ID of the entry of ID `index`.
        fn findId(self: *Self, index: usize) usize {
            const parent: *usize = &self.nodes.items[index].parent;
            if (parent.* != index) parent.* = self.findId(parent.*);
            return parent.*;
        }

        /// Find the root ID of the given value. If it is not yet in the
        /// collection, adds it with `makeSet` and returns the new ID.
        fn find(self: *Self, alloc: std.mem.Allocator, value: T) !usize {
            const id = try self.getMakeId(alloc, value);
            return self.findId(id);
        }

        /// Union operator; combines the sets containing `one` and `two`
        /// together. Returns `false` if they were already merged,
        /// `true` otherwise.
        fn merge(self: *Self, alloc: std.mem.Allocator, one: T, two: T) !bool {
            const one_root = try self.find(alloc, one);
            const two_root = try self.find(alloc, two);
            if (one_root == two_root) return false;

            var small: *Node = &self.nodes.items[one_root];
            var large: *Node = &self.nodes.items[two_root];
            if (small.size > large.size) std.mem.swap(*Node, &small, &large);

            small.parent = large.parent;
            large.size += small.size;
            return true;
        }

        /// Returns the root IDs of each distinct subset in the collection.
        fn rootIds(
            self: *Self,
            alloc: std.mem.Allocator,
        ) !std.ArrayList(usize) {
            var roots: std.ArrayList(usize) = .empty;
            errdefer roots.deinit(alloc);
            var seen: std.AutoHashMapUnmanaged(usize, void) = .empty;
            defer seen.deinit(alloc);

            for (self.ids.values()) |id| {
                const root = self.findId(id);
                if (!seen.contains(root)) {
                    try seen.put(alloc, root, {});
                    try roots.append(alloc, root);
                }
            }

            return roots;
        }

        /// Is the element at index `one` greater than that at index `two`?
        /// Suitable for use with sorting methods such as `std.sort.insertion`.
        fn greaterThan(self: Self, one: usize, two: usize) bool {
            const nodes: []Node = self.nodes.items;
            return nodes[one].size > nodes[two].size;
        }
    };
}

test DisjointSet {
    const alloc = std.testing.allocator;
    const chars = "abcdefghijklm";

    var sets: DisjointSet(u32) = .empty;
    defer sets.deinit(alloc);

    for (chars) |char| {
        _ = try sets.makeSet(alloc, char);
    }
    try std.testing.expectEqual(chars.len, sets.ids.entries.len);

    var roots = try sets.rootIds(alloc);
    defer roots.deinit(alloc);
    try std.testing.expectEqual(chars.len, roots.items.len);

    _ = try sets.merge(alloc, 'a', 'b');
    _ = try sets.merge(alloc, 'a', 'c');
    _ = try sets.merge(alloc, 'b', 'd');
    const a_set_id = try sets.find(alloc, 'a');
    const a_set_size = sets.nodes.items[a_set_id].size;
    try std.testing.expectEqual(4, a_set_size);

    _ = try sets.merge(alloc, 'g', 'i');
    _ = try sets.merge(alloc, 'h', 'j');
    _ = try sets.merge(alloc, 'k', 'l');
    _ = try sets.merge(alloc, 'l', 'm');
    var final = try sets.rootIds(alloc);
    defer final.deinit(alloc);
    try std.testing.expectEqual(chars.len - 7, final.items.len);
}

/// A pair of points.
fn Pair(comptime T: type) type {
    return struct {
        l: Point(T),
        r: Point(T),

        const Self = @This();

        /// Flipped 'compare' operator. Suitable for a max priority queue.
        fn compare(_: void, self: Self, other: Self) std.math.Order {
            return std.math.order(
                other.l.quadrance(other.r),
                self.l.quadrance(self.r),
            );
        }

        /// Is the element at index `one` greater than that at index `two`?
        /// Suitable for use with sorting methods such as `std.sort.insertion`.
        fn lessThan(_: void, one: Self, two: Self) bool {
            return one.l.quadrance(one.r) < two.l.quadrance(two.r);
        }
    };
}

/// A point `(x, y, z)` where each component has type `T`.
fn Point(comptime T: type) type {
    return struct {
        x: T,
        y: T,
        z: T,

        const Self = @This();

        /// Construct directly from an array.
        fn from(array: [3]T) Self {
            return .{ .x = array[0], .y = array[1], .z = array[2] };
        }

        /// Returns the quadrance between two points - the square of the
        /// distance between them, `dx^2 + dy^2 + dz^2`. Cheaper than
        /// calculating distance if *comparison* between distances is all
        /// that's needed.
        fn quadrance(self: Self, other: Self) T {
            const dx: T = @intCast(@abs(self.x - other.x));
            const dy: T = @intCast(@abs(self.y - other.y));
            const dz: T = @intCast(@abs(self.z - other.z));
            return (dx * dx) + (dy * dy) + (dz * dz);
        }

        /// Parse a collection of points from input text formatted like
        ///
        /// ```
        /// 123,456,789
        /// 1111,22,3
        /// ```
        fn parseMany(
            alloc: std.mem.Allocator,
            text: []const u8,
        ) !std.ArrayList(Self) {
            var lines = std.mem.tokenizeScalar(u8, text, '\n');
            var points: std.ArrayList(Self) = .empty;
            errdefer points.deinit(alloc);

            while (lines.next()) |line| {
                var point: Self = undefined;
                var nums = std.mem.tokenizeScalar(u8, line, ',');

                inline for ([_]*T{ &point.x, &point.y, &point.z }) |field| {
                    const slice = nums.next() orelse return error.TooFewNums;
                    field.* = try std.fmt.parseInt(T, slice, 10);
                }
                if (nums.next()) |_| return error.TooManyNums;

                try points.append(alloc, point);
            }

            return points;
        }
    };
}

test "Point.parseMany" {
    const alloc = std.testing.allocator;
    const expected = [_]Point(u32){
        .from(.{ 162, 817, 812 }), .from(.{ 57, 618, 57 }),
        .from(.{ 906, 360, 560 }), .from(.{ 592, 479, 940 }),
        .from(.{ 352, 342, 300 }), .from(.{ 466, 668, 158 }),
        .from(.{ 542, 29, 236 }),  .from(.{ 431, 825, 988 }),
        .from(.{ 739, 650, 466 }), .from(.{ 52, 470, 668 }),
        .from(.{ 216, 146, 977 }), .from(.{ 819, 987, 18 }),
        .from(.{ 117, 168, 530 }), .from(.{ 805, 96, 715 }),
        .from(.{ 346, 949, 466 }), .from(.{ 970, 615, 88 }),
        .from(.{ 941, 993, 340 }), .from(.{ 862, 61, 35 }),
        .from(.{ 984, 92, 344 }),  .from(.{ 425, 690, 689 }),
    };

    var actual = try Point(u32).parseMany(alloc, example);
    defer actual.deinit(alloc);

    try std.testing.expectEqualSlices(Point(u32), &expected, actual.items);
}

/// Our example input for day 8.
const example: []const u8 =
    \\162,817,812
    \\57,618,57
    \\906,360,560
    \\592,479,940
    \\352,342,300
    \\466,668,158
    \\542,29,236
    \\431,825,988
    \\739,650,466
    \\52,470,668
    \\216,146,977
    \\819,987,18
    \\117,168,530
    \\805,96,715
    \\346,949,466
    \\970,615,88
    \\941,993,340
    \\862,61,35
    \\984,92,344
    \\425,690,689
;
