const std = @import("std");

pub fn build(b: *std.Build) void {
    // Standard CLI-configurable target / optimize options:
    // Use: zig build -Dtarget=x86_64-windows-gnu -Doptimize=ReleaseSmall
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "zignes",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Link SDL2
    exe.linkSystemLibrary("SDL2");
    exe.linkLibC();

    // Install the executable to zig-out/bin by default (user chooses prefix).
    b.installArtifact(exe);

    // Convenience "zig build run" step
    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);
}
