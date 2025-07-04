//! Common helper functions that we would otherwise repeat verbatim across
//! multiple files.
const std = @import("std");

/// The max size of an input file in bytes.
pub const max_input_file_bytes = 1_000_000;

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
/// ```sh
/// ZIG_AOC_DAY_01=$(realpath ./txt/day_01.txt) ./zig-out/bin/day-01
/// ```
///
/// Note: this function has an upper limit on the maximum file size it will
/// attempt to read, `max_input_file_bytes`, which acts as a safety measure.
/// It should be more than sufficient for our purposes.
pub fn getInputFile(alloc: std.mem.Allocator, comptime day: u8) ![]u8 {
    const env = std.fmt.comptimePrint("ZIG_AOC_DAY_{d:0>2}", .{day});
    const file = std.posix.getenv(env) orelse return error.MissingEnvVar;

    const handle = try std.fs.openFileAbsoluteZ(file, .{ .mode = .read_only });
    defer handle.close();

    return handle.readToEndAlloc(alloc, max_input_file_bytes);
}

/// An array of arrays, where each array is expected to have the same length.
/// In other words, it should look like a grid. This invariant is always
/// expected to hold true for any value of this type. Provides a few helper
/// methods for convenience.
pub fn GridUnmanaged(comptime T: type) type {
    return struct {
        const Self = @This();

        /// The wrapped data.
        inner: Inner,

        /// The inner type, an array of arrays.
        pub const Inner = std.ArrayListUnmanaged(std.ArrayListUnmanaged(T));

        /// Initialize to an empty grid.
        pub const empty: Self = .{ .inner = .empty };

        /// Initialize to an empty grid with capacity for `num` rows. Caller
        /// owns the returned memory, which can be freed using
        /// the `.deinit(...)` method.
        pub fn initRowCapacity(alloc: std.mem.Allocator, num: usize) !Self {
            return .{ .inner = try Inner.initCapacity(alloc, num) };
        }

        /// Frees the grid - free each array within the outer array, and then
        /// free the outer array itself.
        pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            for (self.inner.items) |*arr| arr.deinit(alloc);
            self.inner.deinit(alloc);
        }
    };
}

/// A coordinate - a position `(x, y)`.
pub fn Coord(comptime T: type) type {
    return struct {
        const Self = @This();

        x: T = 0,
        y: T = 0,

        /// Add two coordinates together.
        pub fn add(self: Self, other: Self) Self {
            return .{
                .x = self.x + other.x,
                .y = self.y + other.y,
            };
        }

        /// Add two coordinates together. Returns null if the result
        /// would go beyond the given bounds.
        pub fn addBound(self: Self, other: Self, bounds: Self) ?Self {
            const new = self.add(other);
            if (new.x >= bounds.x or new.y >= bounds.y) return null;
            return new;
        }

        /// Add two coordinates together. Returns null if the result
        /// would go beyond the given bounds, or go into negative coordinates.
        pub fn addBoundPos(self: Self, other: Self, bounds: Self) ?Self {
            const new = self.addBound(other, bounds) orelse return null;
            if (new.x < 0 or new.y < 0) return null;
            return new;
        }

        /// Subtract one coordinate from another.
        pub fn sub(self: Self, other: Self) Self {
            return .{
                .x = self.x - other.x,
                .y = self.y - other.y,
            };
        }

        /// Subtract one coordinate from another. Returns null if the
        /// result would go into negative coordinates.
        pub fn subPos(self: Self, other: Self) ?Self {
            if (self.x < other.x or self.y < other.y) return null;
            return self.sub(other);
        }

        /// Cast `x` and `y` coordinates to a new type. Returns null if
        /// either value doesn't fit.
        pub fn cast(self: Self, comptime U: type) ?Coord(U) {
            return .{
                .x = std.math.cast(U, self.x) orelse return null,
                .y = std.math.cast(U, self.y) orelse return null,
            };
        }
    };
}
