const std = @import("std");
const Atomic = std.atomic;
const AtomicOrder = std.builtin.AtomicOrder;
const Allocator = std.mem.Allocator;

const NES = @import("nes.zig").NES;
const Screen = @import("screen.zig").Screen;
const Button = @import("controller.zig").Button;
const timing = @import("timing.zig");
const c = @cImport({
    // SDL_cpuinfo.h pulls in arm_neon.h, which zig 0.16's translate-c
    // can't parse (missing __mfp8 builtin). Disable until upstream fixes it.
    @cDefine("SDL_DISABLE_ARM_NEON_H", "1");
    @cInclude("SDL2/SDL.h");
});
const sample_rate = @import("apu.zig").sample_rate;

fn run_thread(
    done: *Atomic.Value(bool),
    paused: *Atomic.Value(bool),
    nes: *NES,
    audio_device: c.SDL_AudioDeviceID,
) void {
    // cycles per frame / cycles per second = seconds per frame; * 1000 for milliseconds.
    const frame_ms: f64 = timing.cpu_cycles_per_frame / timing.cpu_hz * 1000.0; // ~16.64 ms
    var next_frame_ms: f64 = @floatFromInt(c.SDL_GetTicks());
    while (!done.load(AtomicOrder.unordered)) {
        if (paused.load(.unordered)) {
            c.SDL_Delay(1);
            continue;
        }

        const frame_done = nes.tick();
        if (frame_done) {
            const samples = nes.apu.takeSample();

            // negative number reponse means no audio. nothing we can do for the time
            // being so ignore. 0 = healthy
            _ = c.SDL_QueueAudio(audio_device, samples.ptr, @intCast(samples.len * @sizeOf(f32)));
            next_frame_ms += frame_ms;
            const now: f64 = @floatFromInt(c.SDL_GetTicks());
            if (now < next_frame_ms) {
                c.SDL_Delay(@intFromFloat(next_frame_ms - now));
            } else {
                next_frame_ms = now; // fell behind (e.g. after a pause): don't try to catch up
            }
        }
    }
}

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();
    const args = try init.minimal.args.toSlice(allocator);
    const rom_path = if (args.len >= 2) args[1] else {
        std.debug.print("usage: {s} <rom.nes>\n", .{args[0]});
        return error.MissingRomPath;
    };

    var nes: NES = undefined;
    try nes.loadROMFromFile(allocator, init.io, rom_path);
    defer nes.deinit();

    var screen = try Screen.init();
    defer screen.deinit();

    // Audio: Start the audio subsystem
    if (c.SDL_InitSubSystem(c.SDL_INIT_AUDIO) < 0) return error.SDLAudioInit;

    var want = std.mem.zeroes(c.SDL_AudioSpec);
    want.freq = @intFromFloat(sample_rate); // must match rate APU produces
    want.format = c.AUDIO_F32SYS; // 32 bit floats, same as APU.sample()
    want.channels = 1; // the NES is mono
    want.samples = 1024; // SDL's internal buffer size
    want.callback = null; // null = queue mode = we push samples with SDL_QueueAudio

    const audio_dev = c.SDL_OpenAudioDevice(null, 0, &want, null, 0);
    if (audio_dev == 0) return error.SDLAudioOpen;
    defer c.SDL_CloseAudioDevice(audio_dev);

    c.SDL_PauseAudioDevice(audio_dev, 0); // devices start paused, 0 = play

    var done = Atomic.Value(bool).init(false);
    var paused = Atomic.Value(bool).init(false);

    const thread_nes = try std.Thread.spawn(.{}, run_thread, .{
        &done,
        &paused,
        &nes,
        audio_dev,
    });

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
