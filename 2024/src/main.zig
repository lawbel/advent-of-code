//! Convenience module that combines each day.
const day_01 = @import("day_01.zig");
const day_02 = @import("day_02.zig");
const day_03 = @import("day_03.zig");
const day_04 = @import("day_04.zig");
const day_05 = @import("day_05.zig");
const day_06 = @import("day_06.zig");
const day_07 = @import("day_07.zig");
const day_08 = @import("day_08.zig");
const day_09 = @import("day_09.zig");
const std = @import("std");
const utils = @import("utils.zig");

/// Run each day, one after the other.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;

    var buffer: [1024]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    const stdout = &writer.interface;

    const Day = struct { part1: utils.PartFn, part2: utils.PartFn };
    const days = [_]@TypeOf(Day){
        day_01, day_02, day_03, day_04, day_05,
        day_06, day_07, day_08, day_09,
    };

    // Thanks to the above, we can save most of the repetition in running
    // each day. We just have to list the main function in `mains`, and then
    // it gets pulled into this loop.
    inline for (days, 1..) |day, n| {
        const text = try utils.getInputFile(alloc, n);
        defer alloc.free(text);

        if (n > 1) try stdout.print("\n", .{});
        try stdout.print("day {d}:\n", .{n});
        try stdout.flush();

        try stdout.print("  part 1: {d}\n", .{try day.part1(alloc, text)});
        try stdout.flush();

        try stdout.print("  part 2: {d}\n", .{try day.part2(alloc, text)});
        try stdout.flush();
    }
}

// Run all tests for every day. Is is enough to simply reference each module we
// want to test, and the test runner will pick up any tests inside those.
test "all" {
    _ = .{
        day_01, day_02, day_03, day_04, day_05,
        day_06, day_07, day_08, day_09,
    };
}
