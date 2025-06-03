//! Build file - describes how to create and execute the build graph for this
//! project.
const std = @import("std");

/// Populate builder `b` with the build graph.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Provide `zig build day-01` option.
    const run_step = b.step("day-01", "Solve day 1");
    const exe = b.addExecutable(.{
        .name = "day-01",
        .root_source_file = b.path("src/day_01.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    run_cmd.addFileArg(b.path("txt/day_01.txt"));
    run_step.dependOn(&run_cmd.step);

    // Provide `zig build test-01` option.
    const test_step = b.step("test-01", "Test day 1");
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/day_01.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    test_step.dependOn(&run_exe_unit_tests.step);
}
