const std = @import("std");
const Atomic = std.atomic;
const AtomicOrder = std.builtin.AtomicOrder;

const CPU = @import("cpu.zig").CPU;
const Memory = @import("memory.zig").Memory;

fn run(cpu: *CPU) void {
    while (true) { // TODO: Handle cycles per frame
        const halt = cpu.tick();
        if (halt) {
            std.debug.print("CPU halted\n", .{});
        }
    }
}

fn run_thread(done: *Atomic.Value(bool), cpu: *CPU) void {
    while (!done.load(AtomicOrder.unordered)) {
        run(cpu);
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const ram = try allocator.alloc(u8, 0x800);
    var memory = Memory{ .Ram = ram };
    var cpu = CPU.init(&memory);

    var done = Atomic.Value(bool).init(false);

    const thread_nes = try std.Thread.spawn(.{}, run_thread, .{ &done, &cpu });
    defer thread_nes.join();

    done.store(true, .monotonic);

    // Block until thread_nes is done.
}
