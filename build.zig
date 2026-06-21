const std = @import("std");

pub fn build(b: *std.Build) void {
    // Standard CLI-configurable target / optimize options:
    // Use: zig build -Dtarget=x86_64-windows-gnu -Doptimize=ReleaseSmall
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.linkSystemLibrary("SDL2", .{});
    exe_mod.linkSystemLibrary("SDL2_ttf", .{});
    exe_mod.link_libc = true;

    const exe = b.addExecutable(.{
        .name = "zignes",
        .root_module = exe_mod,
    });

    // Install the executable to zig-out/bin by default (user chooses prefix).
    b.installArtifact(exe);

    // Convenience "zig build run" step
    const run_exe = b.addRunArtifact(exe);
    if (b.args) |args| run_exe.addArgs(args);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);

    // WASM build: pure emulator core, no SDL, freestanding.
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });
    const wasm = b.addExecutable(.{
        .name = "zignes",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm.zig"),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
        }),
    });
    wasm.entry = .disabled;
    wasm.rdynamic = true;
    const wasm_install = b.addInstallArtifact(wasm, .{
        .dest_dir = .{ .override = .{ .custom = "wasm" } },
    });
    const wasm_step = b.step("wasm", "Build the WASM module");
    wasm_step.dependOn(&wasm_install.step);
}
