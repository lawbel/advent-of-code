//! Build file - describes how to create and execute the build graph for this
//! project.
const std = @import("std");

/// Populate builder `b` with the build graph.
pub fn build(b: *std.Build) void {
    // Accept standard CLI options.
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // For each day in `days`, we hook up a standalone executable and test
    // suite. We handle the executables and the test suites in separate
    // for-loops to influence the order they get printed when running
    // `zig build --help`. The end result is much more readable than we would
    // get from one loop.
    const days = [_]u8{ 1, 2, 3 };

    // Provide executables for each day.
    inline for (days) |n| {
        // Strings for main executable.
        const name = std.fmt.comptimePrint("day-{d:0>2}", .{n});
        const desc = std.fmt.comptimePrint("Solve day {d}", .{n});
        const src = std.fmt.comptimePrint("src/day_{d:0>2}.zig", .{n});
        const txt = std.fmt.comptimePrint("txt/day_{d:0>2}.txt", .{n});
        const env = std.fmt.comptimePrint("ZIG_AOC_DAY_{d:0>2}", .{n});
        const path = b.pathJoin(&.{ b.build_root.path.?, txt });

        // Provide `zig build day-{n}` option.
        const exe_n = b.addExecutable(.{
            .name = name,
            .root_source_file = b.path(src),
            .target = target,
            .optimize = optimize,
        });
        b.installArtifact(exe_n);

        const run_cmd_n = b.addRunArtifact(exe_n);
        run_cmd_n.step.dependOn(b.getInstallStep());
        run_cmd_n.setEnvironmentVariable(env, path);

        const run_step_n = b.step(name, desc);
        run_step_n.dependOn(&run_cmd_n.step);
    }

    // Provide test suites for each day.
    inline for (days) |n| {
        // Strings for test suite.
        const name = std.fmt.comptimePrint("test-{d:0>2}", .{n});
        const desc = std.fmt.comptimePrint("Test day {d}", .{n});
        const src = std.fmt.comptimePrint("src/day_{d:0>2}.zig", .{n});

        // Provide `zig build test-{n}` option.
        const exe_tests_n = b.addTest(.{
            .root_source_file = b.path(src),
            .target = target,
            .optimize = optimize,
        });
        const run_exe_tests_n = b.addRunArtifact(exe_tests_n);
        const test_step_n = b.step(name, desc);
        test_step_n.dependOn(&run_exe_tests_n.step);
    }
}
