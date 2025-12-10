//! Advent of Code 2024, Day 2: Red-Nosed Reports.
const std = @import("std");
const utils = @import("utils.zig");

/// Run both parts for day 2.
pub fn main() !void {
    try utils.mainDay(2, part1, part2);
}

/// Day 2, part 1.
pub fn part1(alloc: std.mem.Allocator, reports: []const u8) !u64 {
    return countSafe(alloc, reports, std.sort.isSorted);
}

/// Day 2, part 2.
pub fn part2(alloc: std.mem.Allocator, reports: []const u8) !u64 {
    return countSafe(alloc, reports, isApproxSorted);
}

test part1 {
    const alloc = std.testing.allocator;
    const safe = part1(alloc, example_reports);
    try std.testing.expectEqual(2, safe);
}

test part2 {
    const alloc = std.testing.allocator;
    const safe = part2(alloc, example_reports);
    try std.testing.expectEqual(4, safe);
}

/// How many of the given reports are safe?
fn countSafe(
    alloc: std.mem.Allocator,
    reports: []const u8,
    comptime isSorted: @TypeOf(std.sort.isSorted),
) !u64 {
    var list = try parseReports(alloc, reports);
    defer list.deinit(alloc);

    var safe: u64 = 0;
    for (list._0.items) |report| {
        safe += if (reportIsSafe(report.items, isSorted)) 1 else 0;
    }

    return safe;
}

/// A comparison operator, similar to `std.sort.asc`. Checks for `left < right`
/// and that the gap `right - left` is between 1 and 3 (inclusive).
fn slowAsc(comptime T: type) fn (void, T, T) bool {
    return struct {
        fn inner(_: void, r: T, l: T) bool {
            return l > r or (r - l) < 1 or (r - l) > 3;
        }
    }.inner;
}

/// A comparison operator, similar to `std.sort.desc`. Checks for `left > right`
/// and that the gap `left - right` is between 1 and 3 (inclusive).
fn slowDesc(comptime T: type) fn (void, T, T) bool {
    return struct {
        fn inner(_: void, r: T, l: T) bool {
            return r > l or (l - r) < 1 or (l - r) > 3;
        }
    }.inner;
}

/// Is the given report safe or not?
fn reportIsSafe(
    report: []const u64,
    comptime isSorted: @TypeOf(std.sort.isSorted),
) bool {
    const slow_asc = isSorted(u64, report, {}, slowAsc(u64));
    const slow_desc = isSorted(u64, report, {}, slowDesc(u64));
    return slow_asc or slow_desc;
}

test "reportIsSafe std" {
    const alloc = std.testing.allocator;
    const expected: [6]bool = .{ true, false, false, false, false, true };

    var reports = try parseReports(alloc, example_reports);
    defer reports.deinit(alloc);

    var actual: [expected.len]bool = undefined;
    for (reports._0.items, 0..) |report, i| {
        actual[i] = reportIsSafe(report.items, std.sort.isSorted);
    }

    try std.testing.expectEqualSlices(bool, &expected, &actual);
}

test "reportIsSafe approx" {
    const alloc = std.testing.allocator;
    const expected: [6]bool = .{ true, false, false, true, true, true };

    var reports = try parseReports(alloc, example_reports);
    defer reports.deinit(alloc);

    var actual: [expected.len]bool = undefined;
    for (reports._0.items, 0..) |report, i| {
        actual[i] = reportIsSafe(report.items, isApproxSorted);
    }

    try std.testing.expectEqualSlices(bool, &expected, &actual);
}

/// Similar to `std.sort.isSorted`, but allows a single bad element in the
/// list. In other words: returns `true` if the list is sorted, OR if deleting
/// a single element from the list would make it sorted.
fn isApproxSorted(
    comptime T: type,
    items: []const T,
    context: anytype,
    comptime lessThan: fn (context: @TypeOf(context), lhs: T, rhs: T) bool,
) bool {
    var i: usize = 1;
    var faulty = false;
    const n = items.len;

    // Compare elements pairwise along the length of the list, looking for any
    // elements out of order.
    while (i < n) : (i += 1) {
        // Compare item `i - 1` and item `i`.
        const ordered = !lessThan(context, items[i], items[i - 1]);

        // If they are in order, continue merrily on.
        if (ordered) continue;

        // They are out of order and in need of repair. If we've already hit a
        // previous fault - that's too many faults, bail out.
        if (faulty) return false;

        // This is the first fault we've hit. Let's try and repair it. There
        // are exactly two options - skip item `i - 1` or skip item `i`.
        //
        // Note that either option leaves the pairs of elements being compared
        // by this loop unchanged, except for items in the range
        // [i - 2, i - 1, i, i + 1]. So it is enough to check whether each
        // fix works on this range, and if both work it doesn't matter
        // which we pick.
        faulty = true;

        // Attempt fix A: skip item `i - 1`:
        //
        // * That leaves us with pairs (i - 2, i) and (i, i + 1), and no more
        //   faults allowed so both must be properly ordered.
        // * The first pair (i - 2, i) may not exist if we are at the start
        //   of the list.
        // * The second pair (i, i + 1) may not exist if we are at the end of
        //   the list.
        const fix_a_first =
            !(i > 1) or !lessThan(context, items[i], items[i - 2]);
        const fix_a_second =
            !(i + 1 < n) or !lessThan(context, items[i + 1], items[i]);

        if (fix_a_first and fix_a_second) {
            // We've just checked the next pair is okay, so might as well
            // skip it and not do redundant work.
            i += 1;
            continue;
        }

        // Attempt fix B: skip item `i`. The reasoning is much the same as for
        // fix A, so we'll not repeat it in detail. We can skip checking the
        // pair (i - 2, i - 1) at all, as we know it will have been checked
        // last iteration of the loop (if there was a previous iteration).
        const fix_b =
            !(i + 1 < n) or !lessThan(context, items[i + 1], items[i - 1]);

        if (fix_b) {
            // Skip ahead to the next pair (i + 1, i + 2).
            i += 1;
            continue;
        }

        // Neither fix works, so we cannot repair the orderliness of the list.
        return false;
    }

    // We got this far with no faults, or one repairable fault. So, the input
    // list is (mostly) sorted.
    return true;
}

test isApproxSorted {
    const alloc = std.testing.allocator;
    const Result = union(enum) { SlowAsc, SlowDesc, NotSorted };
    const expected: [6]Result =
        .{ .SlowDesc, .NotSorted, .NotSorted, .SlowAsc, .SlowDesc, .SlowAsc };

    var reports = try parseReports(alloc, example_reports);
    defer reports.deinit(alloc);

    var actual: [expected.len]Result = undefined;
    try std.testing.expectEqual(actual.len, reports._0.items.len);
    for (reports._0.items, 0..) |report, i| {
        if (isApproxSorted(u64, report.items, {}, slowAsc(u64))) {
            actual[i] = .SlowAsc;
        } else if (isApproxSorted(u64, report.items, {}, slowDesc(u64))) {
            actual[i] = .SlowDesc;
        } else {
            actual[i] = .NotSorted;
        }
    }

    try std.testing.expectEqualSlices(Result, &expected, &actual);
}

/// Parse a list of reports. Caller owns returned memory, free
/// with `.deinit()` method.
fn parseReports(
    alloc: std.mem.Allocator,
    string: []const u8,
) !utils.GridUnmanaged(u64) {
    // Pre-allocate room for the number of lines in the input. Should save us
    // from re-allocating much if at all.
    const assumed_len = std.mem.count(u8, string, "\n") + 1;
    const Mat64 = utils.GridUnmanaged(u64);
    const Vec64 = std.ArrayListUnmanaged(u64);

    var report = try Mat64.initRowCapacity(alloc, assumed_len);
    errdefer report.deinit(alloc);

    // Iterate over each input row.
    var lines = std.mem.splitScalar(u8, string, '\n');
    while (lines.next()) |line| {
        // Skip any blank lines (especially trailing ones).
        if (line.len < 1) continue;

        // Add a new report to the list.
        var new = try report._0.addOne(alloc);
        new.* = Vec64.empty;

        // Populate that report with each entry.
        var words = std.mem.tokenizeScalar(u8, line, ' ');
        while (words.next()) |word| {
            const val = try std.fmt.parseInt(u64, word, 10);
            try new.append(alloc, val);
        }
    }

    return report;
}

test parseReports {
    const alloc = std.testing.allocator;
    const expected: [6][5]u64 = .{
        .{ 7, 6, 4, 2, 1 },
        .{ 1, 2, 7, 8, 9 },
        .{ 9, 7, 6, 2, 1 },
        .{ 1, 3, 2, 4, 5 },
        .{ 8, 6, 4, 4, 1 },
        .{ 1, 3, 6, 7, 9 },
    };

    var reports = try parseReports(alloc, example_reports);
    defer reports.deinit(alloc);

    for (expected, 0..) |row, i| {
        const actual = reports._0.items[i].items;
        try std.testing.expectEqualSlices(u64, &row, actual);
    }
}

/// The example for day 2.
const example_reports: []const u8 =
    \\7 6 4 2 1
    \\1 2 7 8 9
    \\9 7 6 2 1
    \\1 3 2 4 5
    \\8 6 4 4 1
    \\1 3 6 7 9
;
