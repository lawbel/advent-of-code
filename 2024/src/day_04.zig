//! Advent of Code 2024, Day 4: Ceres Search.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 4.
pub fn main() !void {
    const alloc = std.heap.smp_allocator;
    const stdout = std.io.getStdOut().writer();
    const word = try utils.getInputFile(alloc, 4);
    defer alloc.free(word);

    var search = try asGrid(alloc, u8, word, '\n');
    defer search.deinit(alloc);

    const count = try part1(alloc, search);
    try stdout.print("part 1: {d}\n", .{count});
}

/// Day 4, part 1.
pub fn part1(alloc: std.mem.Allocator, search: utils.GridUnmanaged(u8)) !u32 {
    // Every way of making lines through the word search.
    var nEast = try gridDiags(alloc, u8, .NorthEast, &search);
    defer nEast.deinit(alloc);
    var sEast = try gridDiags(alloc, u8, .SouthEast, &search);
    defer sEast.deinit(alloc);
    var cols = try gridColumns(alloc, u8, &search);
    defer cols.deinit(alloc);
    var rows = gridRows(u8, &search);

    // For each lines in the grid, count 'XMAS' forward and backwards.
    var count: usize = 0;
    while (rows.next()) |row| {
        count += std.mem.count(u8, row, "XMAS");
        count += std.mem.count(u8, row, "SAMX");
    }
    while (cols.next()) |col| {
        count += std.mem.count(u8, col, "XMAS");
        count += std.mem.count(u8, col, "SAMX");
    }
    while (nEast.next()) |diagNE| {
        count += std.mem.count(u8, diagNE, "XMAS");
        count += std.mem.count(u8, diagNE, "SAMX");
    }
    while (sEast.next()) |diagSE| {
        count += std.mem.count(u8, diagSE, "XMAS");
        count += std.mem.count(u8, diagSE, "SAMX");
    }

    return @intCast(count);
}

test part1 {
    const alloc = std.testing.allocator;

    var grid = try asGrid(alloc, u8, example_search, '\n');
    defer grid.deinit(alloc);

    const xmas_count = try part1(alloc, grid);
    try std.testing.expectEqual(18, xmas_count);
}

/// Day 4, part 2.
pub fn part2() !void {}

/// Iterate over the rows in the given grid.
fn gridRows(comptime T: type, grid: *const utils.GridUnmanaged(T)) RowIter(T) {
    const size = grid.inner.items.len;
    return .{
        .grid = grid,
        .size = size,
        .row = if (size > 0) 0 else null,
    };
}

/// An iterator that yields rows, top to bottom.
fn RowIter(comptime T: type) type {
    return struct {
        const Self = @This();

        grid: *const utils.GridUnmanaged(T),
        row: ?usize,
        size: usize,

        /// Get the next row (if any).
        fn next(self: *Self) ?[]const T {
            const n = self.row orelse return null;
            if (n >= self.size) {
                self.row = null;
                return null;
            }

            self.row = n + 1;
            return self.grid.inner.items[n].items;
        }
    };
}

test gridRows {
    const alloc = std.testing.allocator;
    const as_array = [_][]const u8{
        "MMMSXXMASM",
        "MSAMXMSMSA",
        "AMXSXMAAMM",
        "MSAMASMSMX",
        "XMASAMXAMM",
        "XXAMMXXAMA",
        "SMSMSASXSS",
        "SAXAMASAAA",
        "MAMMMXMMMM",
        "MXMXAXMASX",
    };

    var grid = try asGrid(alloc, u8, example_search, '\n');
    defer grid.deinit(alloc);

    var rows = gridRows(u8, &grid);
    var n: usize = 0;
    while (rows.next()) |row| : (n += 1) {
        try std.testing.expectEqualSlices(u8, as_array[n], row);
    }

    try std.testing.expectEqual(as_array.len, n);
}

/// Iterate over the columns in the given grid. Expects the grid to remain
/// alive as long as this iterator is still being used, as we store a reference
/// to it in memory.
///
/// The returned iterator holds allocated memory, and needs freeing by the
/// caller once finished with by using its `.deinit(...)` method.
fn gridColumns(
    alloc: std.mem.Allocator,
    comptime T: type,
    grid: *const utils.GridUnmanaged(T),
) !ColumnIter(T) {
    const size = grid.inner.items.len;
    const buffer = try std.ArrayListUnmanaged(T).initCapacity(alloc, size);

    return .{
        .grid = grid,
        .size = size,
        .buffer = buffer,
        .col = if (size > 0) 0 else null,
    };
}

/// An iterator that yields columns, left to right. It requires freeing once
/// done with, via `.deinit(...)` method.
fn ColumnIter(comptime T: type) type {
    return struct {
        const Self = @This();

        grid: *const utils.GridUnmanaged(T),
        buffer: std.ArrayListUnmanaged(T),
        size: usize,
        col: ?usize,

        /// Get the next column (if any).
        fn next(self: *Self) ?[]const T {
            const col = self.col orelse return null;
            if (col >= self.size) {
                self.col = null;
                return null;
            }

            self.col = col + 1;
            self.buffer.clearRetainingCapacity();
            for (0..self.size) |row| {
                const value = self.grid.inner.items[row].items[col];
                self.buffer.appendAssumeCapacity(value);
            }

            return self.buffer.items;
        }

        /// Free the backing `ArrayList` used by this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.buffer.deinit(alloc);
        }
    };
}

test gridColumns {
    const alloc = std.testing.allocator;
    const transposed = [_][]const u8{
        "MMAMXXSSMM",
        "MSMSMXMAAX",
        "MAXAAASXMM",
        "SMSMSMMAMX",
        "XXXAAMSMMA",
        "XMMSMXAAXX",
        "MSAMXXSSMM",
        "AMASAAXAMA",
        "SSMMMMSAMS",
        "MAMXMASAMX",
    };

    var grid = try asGrid(alloc, u8, example_search, '\n');
    defer grid.deinit(alloc);

    var n: usize = 0;
    var cols = try gridColumns(alloc, u8, &grid);
    defer cols.deinit(alloc);

    while (cols.next()) |row| : (n += 1) {
        try std.testing.expectEqualSlices(u8, transposed[n], row);
    }
    try std.testing.expectEqual(transposed.len, n);
}

/// A diagonal directional.
const Direction = union(enum) { NorthEast, SouthEast };

/// Iterate over the diagonals in the given grid. If you want to get the
/// diagonals which point in a south-easterly direction (from top-left to
/// bottom-right), then pass the argument `diag_dir = .SouthEast`.
/// Otherwise `.NorthEast` will yield the alternate diagonals.
///
/// The returned iterator holds allocated memory, and needs freeing by the
/// caller once finished with by using its `.deinit(...)` method.
fn gridDiags(
    alloc: std.mem.Allocator,
    comptime T: type,
    comptime diag_dir: Direction,
    grid: *const utils.GridUnmanaged(T),
) !DiagIter(T, diag_dir) {
    const size = grid.inner.items.len;
    const buffer = try std.ArrayListUnmanaged(T).initCapacity(alloc, size);

    const init_x: usize = 0;
    const init_y: usize = switch (diag_dir) {
        .SouthEast => size - 1,
        .NorthEast => 0,
    };

    return .{
        .grid = grid,
        .buffer = buffer,
        .size = size,
        .pos_x = if (size > 0) init_x else null,
        .pos_y = if (size > 0) init_y else null,
    };
}

/// An iterator that yields diagonals, either pointing `.NorthEast`
/// or `.SouthEast` depending on the set `diag_dir`.
///
/// It requires freeing once done with, via `.deinit(...)` method.
fn DiagIter(comptime T: type, comptime diag_dir: Direction) type {
    return struct {
        const Self = @This();

        grid: *const utils.GridUnmanaged(T),
        buffer: std.ArrayListUnmanaged(T),
        size: usize,
        pos_x: ?usize,
        pos_y: ?usize,

        /// Get the next diagonal (if any).
        fn next(self: *Self) ?[]const T {
            return switch (diag_dir) {
                .NorthEast => self.northEast(),
                .SouthEast => self.southEast(),
            };
        }

        /// Get the next diagonal pointing north-east (from bottom-left
        /// to top-right), if any.
        fn northEast(self: *Self) ?[]const T {
            // Starting coordinates and length of slice.
            const pos_x = self.pos_x orelse return null;
            const pos_y = self.pos_y orelse return null;
            const len =
                if (pos_y + 1 < self.size) pos_y + 1 else self.size - pos_x;

            // Loop over the diagonal and copy values into buffer.
            self.buffer.clearRetainingCapacity();
            for (0..len) |i| {
                const slice_x = pos_x + i;
                const slice_y = pos_y - i;
                const value = self.grid.inner.items[slice_y].items[slice_x];
                self.buffer.appendAssumeCapacity(value);
            }

            // Set variables for next iteration (if any).
            if (pos_y + 1 < self.size) {
                self.pos_y = pos_y + 1;
            } else if (pos_x + 1 < self.size) {
                self.pos_x = pos_x + 1;
            } else {
                self.pos_x = null;
                self.pos_y = null;
            }

            // Return the diagonal.
            return self.buffer.items;
        }

        /// Get the next diagonal pointing north-east (from bottom-left
        /// to top-right), if any.
        fn southEast(self: *Self) ?[]const T {
            // Starting coordinates and length of slice.
            const pos_x = self.pos_x orelse return null;
            const pos_y = self.pos_y orelse return null;
            const len =
                if (pos_y > 0) self.size - pos_y else self.size - pos_x;

            // Loop over the diagonal and copy values into buffer.
            self.buffer.clearRetainingCapacity();
            for (0..len) |i| {
                const slice_x = pos_x + i;
                const slice_y = pos_y + i;
                const value = self.grid.inner.items[slice_y].items[slice_x];
                self.buffer.appendAssumeCapacity(value);
            }

            // Set variables for next iteration (if any).
            if (pos_y > 0) {
                self.pos_y = pos_y - 1;
            } else if (pos_x + 1 < self.size) {
                self.pos_x = pos_x + 1;
            } else {
                self.pos_x = null;
                self.pos_y = null;
            }

            // Return the diagonal.
            return self.buffer.items;
        }

        /// Free the backing `ArrayList` used by this type.
        fn deinit(self: *Self, alloc: std.mem.Allocator) void {
            self.buffer.deinit(alloc);
        }
    };
}

test "gridDiags(.NorthEast)" {
    const alloc = std.testing.allocator;
    const expected = [_][]const u8{
        "M",
        "MM",
        "ASM",
        "MMAS",
        "XSXMX",
        "XMASXX",
        "SXAMXMM",
        "SMASAMSA",
        "MASMASAMS",
        "MAXMMMMASM",
        "XMASXXSMA",
        "MMMAXAMM",
        "XMASAMX",
        "AXSXMM",
        "XMASA",
        "MMAS",
        "AMA",
        "SM",
        "X",
    };

    var grid = try asGrid(alloc, u8, example_search, '\n');
    defer grid.deinit(alloc);

    var n: usize = 0;
    var diags = try gridDiags(alloc, u8, .NorthEast, &grid);
    defer diags.deinit(alloc);

    while (diags.next()) |diag| : (n += 1) {
        try std.testing.expectEqualSlices(u8, expected[n], diag);
    }
    try std.testing.expectEqual(expected.len, n);
}

test "gridDiags(.SouthEast)" {
    const alloc = std.testing.allocator;
    const expected = [_][]const u8{
        "M",
        "MX",
        "SAM",
        "SAMX",
        "XMXMA",
        "XXSAMX",
        "MMAMMXM",
        "ASAMSAMA",
        "MMASMASMS",
        "MSXMAXSAMX",
        "MASAMXXAM",
        "MMXSXASA",
        "SXMMAMS",
        "XMASMA",
        "XSAMM",
        "MMMX",
        "ASM",
        "SA",
        "M",
    };

    var grid = try asGrid(alloc, u8, example_search, '\n');
    defer grid.deinit(alloc);

    var n: usize = 0;
    var diags = try gridDiags(alloc, u8, .SouthEast, &grid);
    defer diags.deinit(alloc);

    while (diags.next()) |diag| : (n += 1) {
        try std.testing.expectEqualSlices(u8, expected[n], diag);
    }
    try std.testing.expectEqual(expected.len, n);
}

/// Construct a grid from the given `search` string, using `delim` as the
/// delimiter separating rows. Each (non-empty) row is expected to be the same
/// length, othe bnrwise expect to get an `error.RowLengthsDiffer`.
///
/// Caller owns the returned memory which can be freed simply
/// with `grid.deinit(...)`.
fn asGrid(
    alloc: std.mem.Allocator,
    comptime T: type,
    search: []const T,
    delim: T,
) !utils.GridUnmanaged(T) {
    const assumed_len = std.mem.count(T, search, &.{delim}) + 1;
    var grid = try utils.GridUnmanaged(T).initRowCapacity(alloc, assumed_len);
    errdefer grid.deinit(alloc);

    var len: ?usize = null;
    var rows = std.mem.tokenizeScalar(T, search, delim);

    while (rows.next()) |row| {
        if (len) |prev| {
            if (prev != row.len) return error.RowLengthsDiffer;
        } else {
            len = row.len;
        }

        var new = try std.ArrayListUnmanaged(T).initCapacity(alloc, row.len);
        errdefer new.deinit(alloc);
        try new.appendSlice(alloc, row);
        try grid.inner.append(alloc, new);
    }

    return grid;
}

test asGrid {
    const alloc = std.testing.allocator;
    const as_nested_array = [_][]const u8{
        "MMMSXXMASM",
        "MSAMXMSMSA",
        "AMXSXMAAMM",
        "MSAMASMSMX",
        "XMASAMXAMM",
        "XXAMMXXAMA",
        "SMSMSASXSS",
        "SAXAMASAAA",
        "MAMMMXMMMM",
        "MXMXAXMASX",
    };

    var grid = try asGrid(alloc, u8, example_search, '\n');
    defer grid.deinit(alloc);

    try std.testing.expectEqual(as_nested_array.len, grid.inner.items.len);
    for (as_nested_array, grid.inner.items) |expected, actual| {
        try std.testing.expectEqualSlices(u8, expected, actual.items);
    }
}

/// The running example for day 4.
const example_search: []const u8 =
    \\MMMSXXMASM
    \\MSAMXMSMSA
    \\AMXSXMAAMM
    \\MSAMASMSMX
    \\XMASAMXAMM
    \\XXAMMXXAMA
    \\SMSMSASXSS
    \\SAXAMASAAA
    \\MAMMMXMMMM
    \\MXMXAXMASX
;
