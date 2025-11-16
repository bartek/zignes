const std = @import("std");

const CPU = @import("cpu.zig").CPU;
const Memory = @import("memory.zig").Memory;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const ram = try allocator.alloc(u8, 0x800);
    var memory = Memory{ .Ram = ram };
    var cpu = CPU.init();

    _ = cpu.tick(&memory);
}
