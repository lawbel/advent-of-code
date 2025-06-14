//! Common helper functions that we would otherwise repeat verbatim across
//! multiple files.
const std = @import("std");

/// Returns the contents of the input file for the given day. Caller owns
/// returned memory.
///
/// The path to these input file is expected to be communicated over
/// environment variables like `ZIG_AOC_DAY_01`, `ZIG_AOC_DAY_02`, etc.
/// This argument passing is handled automatically by `build.zig` when
/// running e.g. `zig build day-01`. If you are manually calling the compiled
/// binary directly, pass it the absolute path to the text file
/// yourself - something like the below would do.
///
///     ZIG_AOC_DAY_01=$(realpath ./txt/day_01.txt) ./zig-out/bin/day-01
///
/// Note: this function has a set upper limit on the maximum file size it will
/// attempt to read, which acts as a safety measure. It should be more than
/// sufficient for our purposes.
pub fn getInputFile(alloc: std.mem.Allocator, comptime day: u8) ![]u8 {
    const env = std.fmt.comptimePrint("ZIG_AOC_DAY_{d:0>2}", .{day});
    const file = std.posix.getenv(env) orelse return error.MissingEnvVar;

    const handle = try std.fs.openFileAbsoluteZ(file, .{ .mode = .read_only });
    defer handle.close();

    const max_bytes = 1_000_000;
    return handle.readToEndAlloc(alloc, max_bytes);
}
