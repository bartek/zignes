const std = @import("std");
const Atomic = std.atomic;
const AtomicOrder = std.builtin.AtomicOrder;
const Allocator = std.mem.Allocator;

const NES = @import("nes.zig").NES;
const Screen = @import("screen.zig").Screen;
const Button = @import("controller.zig").Button;
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

fn run_thread(done: *Atomic.Value(bool), paused: *Atomic.Value(bool), nes: *NES) void {
    const target_ms: u32 = 16; // ~60 fps (1000/60 ~= 16.67)
    var frame_start: u32 = @intCast(c.SDL_GetTicks());
    while (!done.load(AtomicOrder.unordered)) {
        if (paused.load(.unordered)) {
            c.SDL_Delay(1);
            continue;
        }

        const frame_done = nes.tick();
        if (frame_done) {
            const elapsed: u32 = @as(u32, @intCast(c.SDL_GetTicks())) - frame_start;
            if (elapsed < target_ms) {
                c.SDL_Delay(target_ms - elapsed);
            }
            frame_start = @intCast(c.SDL_GetTicks());
        }
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const args = try std.process.argsAlloc(arena.allocator());
    const rom_path = if (args.len >= 2) args[1] else {
        std.debug.print("usage: {s} <rom.nes>\n", .{args[0]});
        return error.MissingRomPath;
    };

    var nes = try NES.loadROMFromFile(arena.allocator(), rom_path);
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
                    } else if (mapKey(key)) |button| {
                        nes.controller.setButton(button, true);
                    }
                },
                c.SDL_KEYUP => {
                    const key = event.key.keysym.sym;
                    if (mapKey(key)) |button| {
                        nes.controller.setButton(button, false);
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

fn mapKey(key: c_int) ?Button {
    return switch (key) {
        c.SDLK_z => .B,
        c.SDLK_x => .A,
        c.SDLK_RETURN => .Start,
        c.SDLK_RSHIFT => .Select,
        c.SDLK_UP => .Up,
        c.SDLK_DOWN => .Down,
        c.SDLK_LEFT => .Left,
        c.SDLK_RIGHT => .Right,
        else => null,
    };
}
