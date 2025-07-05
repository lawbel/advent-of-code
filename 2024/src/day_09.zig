//! Advent of Code 2024, Day 9: Disk Fragmenter.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 9.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;
    const stdout = std.io.getStdOut().writer();
    const disk = try utils.getInputFile(alloc, 9);
    defer alloc.free(disk);

    const checksum = try part1(alloc, disk);
    try stdout.print("part 1: {d}\n", .{checksum});
}

/// Day 9, part 1 - parse the disk, defrag the drive, and compute its checksum.
pub fn part1(alloc: std.mem.Allocator, text: []const u8) !u64 {
    var disk = try Disk(u64).parse(alloc, text);
    defer disk.deinit(alloc);

    disk.defrag();
    return disk.checksum();
}

/// A disk - a collection of storage blocks, each containing either a file `T`
/// or empty space (null).
fn Disk(comptime T: type) type {
    return struct {
        const Self = @This();

        map: std.ArrayListUnmanaged(?T),

        /// Parse disk from string. Extraneous whitespace is ignored.
        fn parse(alloc: std.mem.Allocator, text: []const u8) !Self {
            const len = text.len;
            if (len == 0) return error.NoInput;

            const State = union(enum) { file, space };
            var n: usize = 0;
            var id: T = 0;
            var map: std.ArrayListUnmanaged(?T) = .empty;
            errdefer map.deinit(alloc);

            loop: switch (State.file) {
                .file => {
                    var digit: bool = false;
                    if (std.fmt.charToDigit(text[n], 10)) |blocks| {
                        try map.appendNTimes(alloc, id, blocks);
                        digit = true;
                        id += 1;
                    } else |_| {}

                    n += 1;
                    if (n < len) continue :loop if (digit) .space else .file;
                },

                .space => {
                    var digit: bool = false;
                    if (std.fmt.charToDigit(text[n], 10)) |free| {
                        try map.appendNTimes(alloc, null, free);
                        digit = true;
                    } else |_| {}

                    n += 1;
                    if (n < len) continue :loop if (digit) .file else .space;
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

        /// Shrink the end of the disk - pop all trailing `null`s.
        fn shrink(self: *Self) void {
            var len: usize = self.map.items.len;
            while (len > 0) : (len -= 1) {
                if (self.map.items[len - 1] != null) break;
            }
            self.map.shrinkRetainingCapacity(len);
        }

        /// Defragment the disk - pop elements from the end, and move them into
        /// available empty slots. Continue this process until the whole disk
        /// is one contiguous block of files.
        fn defrag(self: *Self) void {
            self.shrink();

            var i: usize = 0;
            while (i + 1 < self.map.items.len) : (i += 1) {
                if (self.map.items[i] == null) {
                    self.map.items[i] = self.pop();
                    self.shrink();
                }
            }
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

test "Disk.defrag(small)" {
    const alloc = std.testing.allocator;
    const Int = i64;
    const expected = [_]?Int{ 0, 2, 2, 1, 1, 1, 2, 2, 2 };

    var disk = try Disk(Int).parse(alloc, example_disk_small);
    defer disk.deinit(alloc);
    disk.defrag();
    try std.testing.expectEqualSlices(?Int, &expected, disk.map.items);
}

test "Disk.defrag(large)" {
    const alloc = std.testing.allocator;
    const Int = i32;
    const expected = [_]?Int{
        0, 0, 9, 9, 8, 1, 1, 1, 8, 8, 8, 2, 7, 7,
        7, 3, 3, 3, 6, 4, 4, 6, 5, 5, 5, 5, 6, 6,
    };

    var disk = try Disk(Int).parse(alloc, example_disk_large);
    defer disk.deinit(alloc);
    disk.defrag();
    try std.testing.expectEqualSlices(?Int, &expected, disk.map.items);
}

test "Disk.checksum(large)" {
    const alloc = std.testing.allocator;
    var disk = try Disk(i16).parse(alloc, example_disk_large);
    defer disk.deinit(alloc);
    disk.defrag();
    try std.testing.expectEqual(1928, disk.checksum());
}

/// A small example of a disk map.
const example_disk_small = "12345";

/// A larger example of a disk map.
const example_disk_large = "2333133121414131402";
