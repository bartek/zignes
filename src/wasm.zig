const std = @import("std");
const NES = @import("nes.zig").NES;

// Backing storage of 512 KV for the FixedBufferAllocator (a static buffer). This holds
// NES subsystem allocations (cart, CPU, PPU, PRG/CHR, etc.)
var fba_buf: [512 * 1024]u8 = undefined;
var fba = std.heap.FixedBufferAllocator.init(&fba_buf);

// The actual emulator state.
var nes_storage: NES = undefined;

// Buffer of RGBA pixels.
var frame_buf: [256 * 240 * 4]u8 = undefined;

// Next, are all exports

// JS calls alloc(len) to reserve a buffer, writes ROM bytes into linear memory at
// the returned offset, then calls init(ptr, len).
export fn alloc(len: usize) [*]u8 {
    const slice = fba.allocator().alloc(u8, len) catch unreachable;
    return slice.ptr;
}

export fn init(rom_ptr: [*]const u8, rom_len: usize) bool {
    nes_storage.load(fba.allocator(), rom_ptr[0..rom_len]) catch return false;
    return true;
}

// Run the emulator until the next vblank, then render and return a pointer to
// the 256x240 RGBA framebuffer (245760 bytes).
export fn tick_frame() [*]const u8 {
    while (!nes_storage.tick()) {}
    nes_storage.ppu.render(&frame_buf);
    return &frame_buf;
}

// Bitmask: bit 0 = A, 1 = B, 2 = Select, 3 = Start, 4 = Up, 5 = Down, 6 = Left, 7 = Right
export fn set_buttons(buttons: u8) void {
    nes_storage.controller.button_state.store(buttons, .monotonic);
}
