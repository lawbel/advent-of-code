//! Common helper functions that we would otherwise repeat verbatim across
//! multiple files.
const std = @import("std");

/// Returns the contents of the input file for each day. The path to said file
/// is expected to be communicated over `argv`. Caller owns returned memory.
///
/// The argument passing over `argv` is handled automatically by `build.zig`
/// when running e.g. `zig build day-01`. If you are manually calling the
/// compiled binary directly, pass it the absolute path to the text file over
/// the command line - something like
/// `./zig-out/bin/day-01 $(realpath ./txt/day_01.txt)` will do.
pub fn getInputFile(alloc: std.mem.Allocator) ![]u8 {
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    // Skip first argument (should be program name).
    std.debug.assert(args.skip());
    const file = args.next() orelse return error.MissingArg;

    // Open file and read contents, erroring if it is unexpectedly large.
    const handle = try std.fs.openFileAbsoluteZ(file, .{ .mode = .read_only });
    defer handle.close();

    const max_bytes = 1_000_000;
    return handle.readToEndAlloc(alloc, max_bytes);
}
