//! Convenience module for running/testing all days.
const day_01 = @import("day_01.zig");
const std = @import("std");
const utils = @import("utils.zig");

/// The type of a module with a main function.
const Day = struct { main: fn () anyerror!void };

/// A collection of each day we have implemented.
const days = [_]@TypeOf(Day){
    day_01,
};

/// Run each day, one after the other.
pub fn main() !void {
    var buffer: [1024]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    const stdout = &writer.interface;

    utils.prefix = "  * ";

    inline for (days, 1..) |day, n| {
        if (n > 1) try stdout.print("\n", .{});
        try stdout.print("day {d}:\n", .{n});
        try stdout.flush();
        try day.main();
    }
}

test "all" {
    _ = days;
}
