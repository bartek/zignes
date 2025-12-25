const std = @import("std");
const Atomic = std.atomic;
const AtomicOrder = std.builtin.AtomicOrder;

const CPU = @import("cpu.zig").CPU;
const PPU = @import("ppu.zig").PPU;
const Bus = @import("bus.zig").Bus;
const NESBus = @import("bus.zig").NESBus;
const Screen = @import("screen.zig").Screen;
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

fn run_thread(done: *Atomic.Value(bool), cpu: *CPU) void {
    while (!done.load(AtomicOrder.unordered)) {
        const halt = cpu.tick();
        if (halt) {} // TODO: Handle cycles per frame
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const ram = try allocator.alloc(u8, 0x800);
    var ppu = PPU{};
    const nesBus = NESBus{
        .ram = ram,
        .ppu = &ppu,
    };
    var bus = Bus{ .nesBus = nesBus };
    var cpu = CPU.init(allocator, &bus);

    var screen = try Screen.init();
    defer screen.deinit();

    var done = Atomic.Value(bool).init(false);

    const thread_nes = try std.Thread.spawn(.{}, run_thread, .{ &done, &cpu });

    // Event loop - keep window open
    var quit = false;
    while (!quit) {
        var event: c.SDL_Event = undefined;
        while (c.SDL_PollEvent(&event) != 0) {
            switch (event.type) {
                c.SDL_QUIT => {
                    quit = true;
                    done.store(true, .monotonic);
                },
                else => {},
            }
        }

        try screen.render(&ppu, &bus, &cpu);
        c.SDL_Delay(10);
    }

    done.store(true, .monotonic);
    thread_nes.join();

    // Block until thread_nes is done.
}
