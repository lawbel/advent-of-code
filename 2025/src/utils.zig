//! Repeated helper functions.
const std = @import("std");

/// A placeholder argument, when used with `runDay` it will be replaced with
/// the actual input text for the day as a `[]const u8`.
pub const Input = enum {};

/// A placeholder argument, when used with `runDay` it will be replaced with
/// the same allocator that `runDay` is given.
pub const Alloc = enum {};

/// A suitable general purpose allocator to be used for every day.
pub const default_alloc = std.heap.smp_allocator;

/// A prefix string used when printing each part in `runDay`.
pub var prefix: []const u8 = "";

/// Run both parts for the given day. Handles fetching the input text and
/// printing the output with some basic formatting. Expected usage looks like:
///
/// ```
/// runDay(
///     .{ .day = 12 },
///     .{ part1, Input },
///     .{ part2, Alloc, Input, true },
/// );
/// ```
///
/// This will fetch the input for day 12 and then call the given functions
/// with those arguments, replacing any instances of `Input` / `Alloc` with
/// the input text / the given allocator and passing along all other arguments
/// unchanged. So this example results in the calls
///
/// ```
/// const input = try getInputFile(alloc, 12);
/// try part1(input);
/// try part2(alloc, input, true);
/// ```
pub fn runDay(
    opts: struct {
        day: comptime_int,
        alloc: std.mem.Allocator = default_alloc,
    },
    part1: anytype,
    part2: anytype,
) !void {
    var buffer: [1024]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    const stdout = &writer.interface;

    const input = try getInputFile(opts.alloc, opts.day);
    defer opts.alloc.free(input);

    inline for (.{ part1, part2 }, 1..) |part, num| {
        const Args = try ArgTuple(part);
        var args: Args = undefined;
        inline for (0..@typeInfo(Args).@"struct".fields.len) |i| {
            args[i] = switch (part[i + 1]) {
                Alloc => opts.alloc,
                Input => input,
                else => |val| val,
            };
        }

        const fun = part[0];
        const result = try @call(.auto, fun, args);

        try stdout.print("{s}part {d}: {}\n", .{ prefix, num, result });
        try stdout.flush();
    }
}

/// Helper for `runDay`. Give it a bundle of function and arguments
/// `.{fn, arg1, arg2, ...}` and it will return the type of the arguments
/// as a tuple `.{Arg1Type, Arg2Type, ...}`. If any argument is one of the
/// placeholder types `Input` or `Alloc`, we'll set the type of those
/// arguments as `[]const u8` and `std.mem.Allocator` instead.
fn ArgTuple(part: anytype) !type {
    const Type = @TypeOf(part);
    const info = @typeInfo(Type);
    if (info != .@"struct" or !info.@"struct".is_tuple) {
        @compileError("expected tuple, found " ++ @typeName(Type));
    }

    const count = info.@"struct".fields.len - 1;
    var fields: [count]std.builtin.Type.StructField = undefined;
    inline for (&fields, 1..) |*field, i| {
        const New = switch (part[i]) {
            Input => []const u8,
            Alloc => std.mem.Allocator,
            else => |orig| @TypeOf(orig),
        };
        field.* = .{
            .name = std.fmt.comptimePrint("{d}", .{i - 1}),
            .type = New,
            .alignment = @alignOf(New),
            .is_comptime = false,
            .default_value_ptr = null,
        };
    }

    return @Type(.{
        .@"struct" = .{
            .layout = .auto,
            .is_tuple = true,
            .fields = &fields,
            .decls = &.{},
        },
    });
}

/// The max size of an input file.
pub const max_input_bytes = 1_000_000;

/// Returns the contents of the input file for the given day. Caller owns
/// returned memory.
///
/// The path to these input file is expected to be communicated over
/// environment variables like `ZIG_AOC_DAY_01`, `ZIG_AOC_DAY_02`, etc.
/// This argument passing is handled automatically by `build.zig` when
/// running e.g. `zig build run-day-12`. If you are manually calling the
/// compiled binary directly, pass it the absolute path to the text file
/// yourself - something like the below would do.
///
/// ```sh
/// ZIG_AOC_DAY_12=$(realpath ./txt/day_12.txt) ./zig-out/bin/day-12
/// ```
///
/// Note: this function has an upper limit on the maximum file size it will
/// attempt to read, `max_input_bytes`, which acts as a safety measure.
/// It should not present any inconvenience for purposes of this project.
pub fn getInputFile(alloc: std.mem.Allocator, comptime day: u8) ![]u8 {
    const env = std.fmt.comptimePrint("ZIG_AOC_DAY_{d:0>2}", .{day});
    const file = std.posix.getenv(env) orelse return error.MissingEnvVar;

    const handle = try std.fs.openFileAbsoluteZ(file, .{ .mode = .read_only });
    defer handle.close();

    return handle.readToEndAlloc(alloc, max_input_bytes);
}
