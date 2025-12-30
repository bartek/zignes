const std = @import("std");
const Atomic = std.atomic;
const AtomicOrder = std.builtin.AtomicOrder;
const Allocator = std.mem.Allocator;

const NES = @import("nes.zig").NES;
const Screen = @import("screen.zig").Screen;
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

fn run_thread(done: *Atomic.Value(bool), paused: *Atomic.Value(bool), nes: *NES) void {
    while (!done.load(AtomicOrder.unordered)) {
        const is_paused: bool = paused.load(AtomicOrder.unordered);
        if (!is_paused) {
            nes.tick();
        }
    }
}

pub fn main() !void {
    var nes = try NES.loadROMFromFile(std.heap.page_allocator, "roms/donkeykong.nes");
    defer nes.deinit();

    var screen = try Screen.init();
    defer screen.deinit();

    var done = Atomic.Value(bool).init(false);
    var paused = Atomic.Value(bool).init(false);

    const thread_nes = try std.Thread.spawn(.{}, run_thread, .{ &done, &paused, &nes });

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
                c.SDL_KEYDOWN => {
                    const key = event.key.keysym.sym;
                    if (key == c.SDLK_SPACE) {
                        const current = paused.load(AtomicOrder.unordered);
                        paused.store(!current, AtomicOrder.monotonic);
                    }
                },
                else => {},
            }
        }

        try screen.render(&nes);
        c.SDL_Delay(10);
    }

    done.store(true, .monotonic);
    thread_nes.join();

    // Block until thread_nes is done.
}
