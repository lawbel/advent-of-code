//! Convenience module that combines each day.
const day_01 = @import("./day_01.zig");
const day_02 = @import("./day_02.zig");
const day_03 = @import("./day_03.zig");
const std = @import("std");

/// Run each day, one after the other.
pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    // Even if each day has a different error union that it can return, it is
    // still okay to combine them in an array like this - as long as they all
    // are of the form `fn () ...!void` they can be unified to a common type.
    const MainType = @TypeOf(main);
    const mains = [_]MainType{
        day_01.main,
        day_02.main,
        day_03.main,
    };

    // Thanks to the above, we can save most of the repetition in running
    // each day. We just have to list the main function in `mains`, and then
    // it gets pulled into this loop.
    inline for (mains, 1..) |main_fn, day| {
        if (day > 1) try stdout.print("\n", .{});
        try stdout.print("> day {d}\n", .{day});
        try main_fn();
    }
}

// Run all tests for every day.
test "all" {
    _ = day_01;
    _ = day_02;
    _ = day_03;
}
