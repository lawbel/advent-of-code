//! Convenience module for running/testing all days.
const day_01 = @import("day_01.zig");
const day_02 = @import("day_02.zig");
const day_03 = @import("day_03.zig");
const day_04 = @import("day_04.zig");
const day_05 = @import("day_05.zig");
const day_06 = @import("day_06.zig");
const day_07 = @import("day_07.zig");
const std = @import("std");
const utils = @import("utils.zig");

/// The type of a module with a main function.
const Day = struct { main: fn () anyerror!void };

/// A collection of each day we have implemented.
const days = [_]@TypeOf(Day){
    day_01, day_02, day_03, day_04,
    day_05, day_06, day_07,
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
_ = utils;
}
