//! Build file - describes how to create and execute the build graph for this
//! project.
const std = @import("std");

/// Populate builder `b` with the build graph.
pub fn build(b: *std.Build) void {
    // Accept standard CLI options.
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // For each day in `days = [_]u8{ 1, 2, ..., max }`, we will hook up a
    // standalone executable and test suite.
    const days = comptime init: {
        const max = 10;
        var list: [max]u8 = undefined;
        for (&list, 1..) |*day, n| day.* = n;
        break :init list;
    };

    // Provide `zig build all` option that runs each day, one after the
    // other in sequence. We pass input files over environment variables.
    b.step("all", "Run all days").dependOn(step: {
        const exe = b.addExecutable(.{
            .name = "all",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        b.installArtifact(exe);

        const run = b.addRunArtifact(exe);
        run.step.dependOn(&exe.step);
        inline for (days) |n| {
            const env = std.fmt.comptimePrint("ZIG_AOC_DAY_{d:0>2}", .{n});
            const txt = std.fmt.comptimePrint("txt/day_{d:0>2}.txt", .{n});
            const path = b.pathJoin(&.{ b.build_root.path.?, txt });
            run.setEnvironmentVariable(env, path);
        }

        break :step &run.step;
    });

    // Provide `zig build tests` option to run all tests in each day. This is
    // nice to have as a quick way to test the whole project.
    b.step("tests", "Test all days").dependOn(step: {
        const tests = b.addTest(.{
            .name = "all",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        const run = b.addRunArtifact(tests);
        break :step &run.step;
    });

    // Provide options `zig build day-{n}` to run executables for each day.
    inline for (days) |n| {
        const name = std.fmt.comptimePrint("day-{d:0>2}", .{n});
        const desc = std.fmt.comptimePrint("Run day {d}", .{n});

        b.step(name, desc).dependOn(step: {
            const src = std.fmt.comptimePrint("src/day_{d:0>2}.zig", .{n});
            const exe = b.addExecutable(.{
                .name = name,
                .root_module = b.createModule(.{
                    .root_source_file = b.path(src),
                    .target = target,
                    .optimize = optimize,
                }),
            });
            b.installArtifact(exe);

            const env = std.fmt.comptimePrint("ZIG_AOC_DAY_{d:0>2}", .{n});
            const txt = std.fmt.comptimePrint("txt/day_{d:0>2}.txt", .{n});
            const path = b.pathJoin(&.{ b.build_root.path.?, txt });
            const run = b.addRunArtifact(exe);
            run.step.dependOn(&exe.step);
            run.setEnvironmentVariable(env, path);

            break :step &run.step;
        });
    }

    // Provide options `zig build test-{n}` to run test suites for each day.
    inline for (days) |n| {
        // Need some extra branching to do the comptime string manipulation.
        @setEvalBranchQuota(30_000);
        const name = std.fmt.comptimePrint("test-{d:0>2}", .{n});
        const desc = std.fmt.comptimePrint("Test day {d}", .{n});

        b.step(name, desc).dependOn(step: {
            const src = std.fmt.comptimePrint("src/day_{d:0>2}.zig", .{n});
            const tests = b.addTest(.{
                .name = name,
                .root_module = b.createModule(.{
                    .root_source_file = b.path(src),
                    .target = target,
                    .optimize = optimize,
                }),
            });
            const run = b.addRunArtifact(tests);
            break :step &run.step;
        });
    }
}
