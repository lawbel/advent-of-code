//! Advent of Code 2024, Day 9: Disk Fragmenter.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 9.
pub fn main() !void {
    try utils.mainDay(9, part1, part2);
}

/// Day 9, part 1 - parse the disk, compact the drive, and compute its checksum.
pub fn part1(alloc: std.mem.Allocator, text: []const u8) !u64 {
    var disk = try Disk(u64).parse(alloc, text);
    defer disk.deinit(alloc);

    disk.compact();
    return disk.checksum();
}

/// Day 9, part 2 - parse the disk, defrag the drive, and compute its checksum.
pub fn part2(alloc: std.mem.Allocator, text: []const u8) !u64 {
    var disk = try Disk(u64).parse(alloc, text);
    defer disk.deinit(alloc);

    disk.defrag();
    return disk.checksum();
}

/// A disk - a collection of storage blocks, each containing either a file
/// segment `T` or empty space `null`.
fn Disk(comptime T: type) type {
    return struct {
        const Self = @This();

        map: std.ArrayListUnmanaged(?T),

        /// Parse disk from string. Extraneous whitespace is ignored.
        fn parse(alloc: std.mem.Allocator, text: []const u8) !Self {
            const State = union(enum) { file, space };
            const len = text.len;
            if (len == 0) return error.NoInput;

            var n: usize = 0;
            var id: T = 0;
            var map: std.ArrayListUnmanaged(?T) = .empty;
            errdefer map.deinit(alloc);

            loop: switch (State.file) {
                .file => {
                    n += 1;
                    if (std.fmt.charToDigit(text[n - 1], 10)) |blocks| {
                        try map.appendNTimes(alloc, id, blocks);
                        id += 1;
                        if (n < len) continue :loop .space;
                    } else |_| {
                        if (n < len) continue :loop .file;
                    }
                },

                .space => {
                    n += 1;
                    if (std.fmt.charToDigit(text[n - 1], 10)) |free| {
                        try map.appendNTimes(alloc, null, free);
                        if (n < len) continue :loop .file;
                    } else |_| {
                        if (n < len) continue :loop .space;
                    }
                },
            }

            return .{ .map = map };
        }

        /// Free the memory backing this disk.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.map.deinit(alloc);
        }

        /// Pop the last block from the disk.
        fn pop(self: *Self) ?T {
            return self.map.pop() orelse null;
        }

        /// Get the start index of the file ending just before `stop`. So this
        /// file occupies the slice `self.map.items[start..stop]`.
        fn fileStart(self: Self, stop: usize) !usize {
            if (stop > self.map.items.len) return error.OutOfBounds;
            const id = self.map.items[stop - 1] orelse return error.FileIsNull;

            var n: usize = stop;
            while (n > 0) : (n -= 1) {
                if (self.map.items[n - 1] != id) break;
            }

            return n;
        }

        /// Find the previous file - iterate backwards from `pos` searching for
        /// any file with ID smaller than `id`, or any file at all
        /// if `id == null`. Returns the next index `n`, so that the file
        /// found occupies a slice like `self.map.items[?? .. n]`.
        fn prevFile(self: Self, pos: usize, id: ?T) ?usize {
            if (pos > self.map.items.len) return null;

            var n: usize = pos;
            while (n > 0) : (n -= 1) {
                const file = self.map.items[n - 1] orelse continue;
                if (id == null or file < id.?) return n;
            }

            return null;
        }

        /// Finds a contiguous section of free (null) memory of length `len`
        /// and returns the starting index. If a limit is given search within
        /// the range `self.map.items[0..limit]`, otherwise search the entire
        /// disk `self.map.items`.
        fn freeSpace(self: Self, len: usize, limit: ?usize) ?usize {
            const items = self.map.items.len;
            const size = if (limit) |lim| @min(lim, items) else items;

            var start: usize = 0;
            while (start + len <= size) : (start += 1) {
                const slice = self.map.items[start .. start + len];
                if (std.mem.allEqual(?T, slice, null)) return start;
            }

            return null;
        }

        /// Defragment the disk:
        ///
        /// * move the last file on disk to available free (null) space at
        ///   the front of the disk;
        /// * if there isn't a big enough section of free memory to accomodate
        ///   the file, leave it as-is;
        /// * repeat this process, iterating through every file from the back
        ///   of the disk all the way to front.
        ///
        /// We take care not to move the same file more than once.
        fn defrag(self: *Self) void {
            self.shrink();

            var stop = self.map.items.len;
            while (stop > 0) {
                const id = self.map.items[stop - 1] orelse unreachable;
                const start = self.fileStart(stop) catch unreachable;
                const size = stop - start;

                if (self.freeSpace(size, start)) |free| {
                    @memset(self.map.items[free .. free + size], id);
                    @memset(self.map.items[start..stop], null);
                }
                stop = self.prevFile(start, id) orelse 0;
            }

            self.shrink();
        }

        /// Compact the disk - pop elements from the end, and move them into
        /// available empty slots. Continue this process until the whole disk
        /// is one contiguous array of files.
        fn compact(self: *Self) void {
            self.shrink();

            var i: usize = 0;
            while (i + 1 < self.map.items.len) : (i += 1) {
                if (self.map.items[i] == null) {
                    self.map.items[i] = self.pop();
                    self.shrink();
                }
            }
        }

        /// Shrink the end of the disk - pop all trailing `null`s.
        fn shrink(self: *Self) void {
            const len = self.prevFile(self.map.items.len, null) orelse 0;
            self.map.shrinkRetainingCapacity(len);
        }

        /// Calculate a file-system checksum.
        fn checksum(self: Self) T {
            var sum: T = 0;
            var i: T = 0;

            for (self.map.items) |maybe| {
                if (maybe) |val| sum += (val * i);
                i += 1;
            }

            return sum;
        }
    };
}

test "Disk.parse(small)" {
    const alloc = std.testing.allocator;
    const Int = u8;
    const expected = [_]?Int{
        0, null, null, 1, 1, 1, null, null, null, null, 2, 2, 2, 2, 2,
    };

    var disk = try Disk(Int).parse(alloc, example_disk_small);
    defer disk.deinit(alloc);
    try std.testing.expectEqualSlices(?Int, &expected, disk.map.items);
}

test "Disk.parse(large)" {
    const alloc = std.testing.allocator;
    const Int = u16;
    const expected = [_]?Int{
        0, 0,    null, null, null, 1, 1, 1,    null, null, null,
        2, null, null, null, 3,    3, 3, null, 4,    4,    null,
        5, 5,    5,    5,    null, 6, 6, 6,    6,    null, 7,
        7, 7,    null, 8,    8,    8, 8, 9,    9,
    };

    var disk = try Disk(Int).parse(alloc, example_disk_large);
    defer disk.deinit(alloc);
    try std.testing.expectEqualSlices(?Int, &expected, disk.map.items);
}

test "Disk.compact(small)" {
    const alloc = std.testing.allocator;
    const Int = i64;
    const expected = [_]?Int{ 0, 2, 2, 1, 1, 1, 2, 2, 2 };

    var disk = try Disk(Int).parse(alloc, example_disk_small);
    defer disk.deinit(alloc);

    disk.compact();
    try std.testing.expectEqualSlices(?Int, &expected, disk.map.items);
}

test "Disk.compact(large)" {
    const alloc = std.testing.allocator;
    const Int = i32;
    const expected = [_]?Int{
        0, 0, 9, 9, 8, 1, 1, 1, 8, 8, 8, 2, 7, 7,
        7, 3, 3, 3, 6, 4, 4, 6, 5, 5, 5, 5, 6, 6,
    };

    var disk = try Disk(Int).parse(alloc, example_disk_large);
    defer disk.deinit(alloc);

    disk.compact();
    try std.testing.expectEqualSlices(?Int, &expected, disk.map.items);
    try std.testing.expectEqual(1928, disk.checksum());
}

test "Disk.fileStart(large)" {
    const alloc = std.testing.allocator;
    const Int = u32;
    const id: Int = 9;
    const size: usize = 2;
    const slice: [size]?Int = .{id} ** size;

    var disk = try Disk(Int).parse(alloc, example_disk_large);
    defer disk.deinit(alloc);

    const stop = disk.map.items.len;
    const start = try disk.fileStart(stop);
    const file = disk.map.items[start..stop];
    try std.testing.expectEqual(stop - size, start);
    try std.testing.expectEqualSlices(?Int, &slice, file);
}

test "Disk.freeSpace(small)" {
    const alloc = std.testing.allocator;
    var disk = try Disk(usize).parse(alloc, example_disk_small);
    defer disk.deinit(alloc);

    try std.testing.expectEqual(null, disk.freeSpace(5, null));
    try std.testing.expectEqual(6, disk.freeSpace(4, null));
    try std.testing.expectEqual(6, disk.freeSpace(3, null));
    try std.testing.expectEqual(1, disk.freeSpace(2, null));
}

test "Disk.freeSpace(large)" {
    const alloc = std.testing.allocator;
    var disk = try Disk(u64).parse(alloc, example_disk_large);
    defer disk.deinit(alloc);

    try std.testing.expectEqual(null, disk.freeSpace(4, null));
    try std.testing.expectEqual(2, disk.freeSpace(3, null));
}

test "Disk.defrag(large)" {
    const alloc = std.testing.allocator;
    const Int = usize;
    const expected = [_]?Int{
        0,    0,    9,    9,    2,    1,    1,    1, 7,    7,
        7,    null, 4,    4,    null, 3,    3,    3, null, null,
        null, null, 5,    5,    5,    5,    null, 6, 6,    6,
        6,    null, null, null, null, null, 8,    8, 8,    8,
    };

    var disk = try Disk(Int).parse(alloc, example_disk_large);
    defer disk.deinit(alloc);

    disk.defrag();
    try std.testing.expectEqualSlices(?Int, &expected, disk.map.items);
    try std.testing.expectEqual(2858, disk.checksum());
}

/// A small example of a disk map.
const example_disk_small = "12345";

/// A larger example of a disk map.
const example_disk_large = "2333133121414131402";
