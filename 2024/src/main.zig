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

/// Run each day, one after the other.
pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const mains = [_]fn () anyerror!void{
        day_01.main, day_02.main, day_03.main, day_04.main, day_05.main,
        day_06.main, day_07.main, day_08.main, day_09.main,
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

// Run all tests for every day. Is is enough to simply reference each module we
// want to test, and the test runner will pick up any tests inside those.
test "all" {
    _ = .{
        day_01, day_02, day_03, day_04, day_05,
        day_06, day_07, day_08, day_09,
    };
}
