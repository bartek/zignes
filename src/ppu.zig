const std = @import("std");
const Cartridge = @import("cartridge.zig").Cartridge;

pub const VramAddr = packed struct {
    coarse_x: u5 = 0, // bits 0–4
    coarse_y: u5 = 0, // bits 5–9
    nametable: u2 = 0, // bits 10–11
    fine_y: u3 = 0, // bits 12–14
    comptime {
        std.debug.assert(@bitSizeOf(VramAddr) == 15);
    }
};

pub const PPU = struct {
    ppu_ctrl: PPUCTRL = .{},
    ppu_mask: PPUMask = .{},

    // OAM Registers & Memory
    oam_addr: u8 = 0, // $2003 write
    vram: [2048]u8 = [_]u8{0} ** 2048, // 2KB internal VRAM
    palette: [32]u8 = [_]u8{0} ** 32, // 32 bytes of palette RAM
    data_buffer: u8 = 0, // Used for delayed reads from VRAM

    // Loopy registers. These render the background and support scrolling
    v: VramAddr = .{}, // $2006 write
    t: VramAddr = .{}, // temporary register for $2006 writes
    w: u1 = 0, // Write latch (0 = first write, 1 = second write) for $2006

    cart: ?*const Cartridge = null,

    cycle: u16 = 0,
    scanline: u16 = 0,

    // read a byte from PPU memory
    fn ppuRead(self: *PPU, addr: u16) u8 {
        const mapped = addr & 0x3fff; // Mirror addresses above $3FFF down to $0000–$3FFF
        return switch (mapped) {
            // Pattern tables, chr-rom from the cartridge
            0x0000...0x1fff => {
                if (self.cart) |c| return c.chr_rom[mapped] else return 0;
            },
            0x2000...0x3eff => {
                return self.vram[self.mirrorVramAddr(mapped)];
            },
            0x3f00...0x3fff => {
                var pal_addr = mapped & 0x001f;
                if (pal_addr >= 0x10 and pal_addr % 4 == 0) pal_addr -= 0x10;
                return self.palette[pal_addr];
            },
            else => unreachable,
        };
    }

    fn ppuWrite(self: *PPU, addr: u16, val: u8) void {
        const mapped = addr & 0x3fff;
        switch (mapped) {
            0x0000...0x1fff => return, // Usually read-only CHR-ROM
            0x2000...0x3eff => {
                self.vram[self.mirrorVramAddr(mapped)] = val;
            },
            0x3f00...0x3fff => {
                var pal_addr = mapped & 0x001f;
                if (pal_addr >= 0x10 and pal_addr % 4 == 0) pal_addr -= 0x10;
                self.palette[pal_addr] = val;
            },
            else => unreachable,
        }
    }

    fn mirrorVramAddr(self: *const PPU, addr: u16) u16 {
        const mirrored = addr & 0x0FFF;
        const is_vertical = if (self.cart) |c| c.header.flags_6.mirroring_is_vertical else false;

        if (is_vertical) {
            return mirrored & 0x07FF;
        } else {
            return ((mirrored >> 1) & 0x0400) | (mirrored & 0x03FF);
        }
    }

    pub fn readRegister(self: *PPU, addr: u16) u8 {
        std.debug.assert(addr >= 0 and addr <= 8);

        return switch (addr) {
            2 => { // PPUSTATUS
                // Reset address latch
                self.w = 0;
                return 0; // TODO: Return actual status flags
            },
            7 => { // PPUDATA
                const current_v: u15 = @bitCast(self.v);
                var result = self.data_buffer;
                self.data_buffer = self.ppuRead(current_v);

                // Palette reads bypass the buffer
                if (current_v >= 0x3F00) {
                    result = self.data_buffer;
                }

                const inc: u15 = if (self.ppu_ctrl.vram_increment == 0) 1 else 32;
                self.v = @bitCast(current_v + inc);

                return result;
            },
            0, 1, 3, 4, 5, 6, 8 => {
                _ = self.oam_addr;
                // use oam_addr
                return 0;
            },
            else => unreachable,
        };
    }

    pub fn writeRegister(self: *PPU, addr: u16, val: u8) void {
        std.debug.assert(addr >= 0 and addr <= 8);

        switch (addr) {
            0 => { // PPUCTRL
                self.ppu_ctrl = @bitCast(val);
                // Also update the nametable bits in the temporary register `t`
                self.t.nametable = self.ppu_ctrl.nametable;
            },
            1 => self.ppu_mask = @bitCast(val), // PPUMASK
            2 => return, // PPUSTATUS is read only
            3 => self.oam_addr = val, // OAMADDR
            6 => { // PPUADDR
                const t_val: u15 = @bitCast(self.t);
                if (self.w == 0) {
                    // First write: set high byte of t, clear bit 14 (which is mapped to bit 15 here, so it's a u15)
                    const cleared_high = t_val & 0x00FF;
                    const new_high = @as(u15, val & 0x3F) << 8;
                    self.t = @bitCast(cleared_high | new_high);
                    self.w = 1;
                } else {
                    // Second write: set low byte of t, then v = t
                    const cleared_low = t_val & @as(u15, 0x7F00);
                    self.t = @bitCast(cleared_low | val);
                    self.v = self.t;
                    self.w = 0;
                }
            },
            7 => { // PPUDATA
                const current_v: u15 = @bitCast(self.v);
                self.ppuWrite(current_v, val);

                const inc: u15 = if (self.ppu_ctrl.vram_increment == 0) 1 else 32;
                self.v = @bitCast(current_v + inc);
            },
            4, 5, 8 => {
                // TODO: other registers
                return;
            },
            else => unreachable,
        }
    }

    // CPU clock is 3x slower than PPU clock
    pub fn tick(self: *PPU) void {
        // TODO: Draw a pixel now?
        self.cycle += 1;
        // magic number is physical boundary
        if (self.cycle >= 341) {
            self.cycle = 0;
            self.scanline += 1;
            if (self.scanline >= 261) {
                self.scanline = 0;
            }
        }
    }

    // Render PPU framebuffer to pixel data
    // Returns a buffer of (256 * 240 * 4) bytes in RGBA format
    pub fn render(self: *const PPU, buffer: []u8) void {
        self.debugRenderPatternTable(buffer);
    }

    // Render all 512 tiles from CHR-ROM to the screen. This mostly exists for
    // debugging/tinkering early on in development.
    pub fn debugRenderPatternTable(self: *const PPU, buffer: []u8) void {
        @memset(buffer, 0);

        const cart = self.cart orelse return;
        const chr = cart.chr_rom;
        if (chr.len == 0) return;

        // Pattern table 0 starts at $0000, Table 1 at $1000
        // Each table is 256 tiles, each tile is 16 bytes
        for (0..512) |tile_idx| {
            const tile_x = tile_idx % 32; // 32 tiles per row (256 pixels)
            const tile_y = tile_idx / 32;

            const base = tile_idx * 16;
            if (base + 16 > chr.len) break;

            for (0..8) |y| {
                const low_byte = chr[base + y]; // Plane 0
                const high_byte = chr[base + y + 8]; // Plane 1

                for (0..8) |x| {
                    const bit0 = (low_byte >> @intCast(7 - x)) & 1;
                    const bit1 = (high_byte >> @intCast(7 - x)) & 1;
                    const color_idx = (bit1 << 1) | bit0;

                    const color: u32 = switch (color_idx) {
                        0 => 0x000000FF, // Black
                        1 => 0xAAAAAAFF, // Gray
                        2 => 0xDDDDDDFF, // Light Gray
                        3 => 0xFFFFFFFF, // White
                        else => unreachable,
                    };

                    const pixel_x = (tile_x * 8) + x;
                    const pixel_y = (tile_y * 8) + y;

                    if (pixel_x < 256 and pixel_y < 240) {
                        const offset = (pixel_y * 256 + pixel_x) * 4;
                        buffer[offset + 0] = @intCast((color >> 24) & 0xFF); // R
                        buffer[offset + 1] = @intCast((color >> 16) & 0xFF); // G
                        buffer[offset + 2] = @intCast((color >> 8) & 0xFF); // B
                        buffer[offset + 3] = @intCast(color & 0xFF); // A
                    }
                }
            }
        }
    }
};

// 7  bit  0
// ---- ----
// VPHB SINN
// |||| ||||
// |||| ||++- nametable (u2)
// |||| |+--- vram_increment
// |||| +---- sprite_pattern
// |||+------ background_pattern
// ||+------- sprite_size
// |+-------- master_slave
// +--------- nmi_enable
//
const PPUCTRL = packed struct {
    nametable: u2 = 0, // bits 0–1
    vram_increment: u1 = 0, // bit 2
    sprite_pattern: u1 = 0, // bit 3
    background_pattern: u1 = 0, // bit 4
    sprite_size: u1 = 0, // bit 5
    master_slave: u1 = 0, // bit 6
    nmi_enable: u1 = 0, // bit 7
};

// 7  bit  0
// ---- ----
// BGRs bMmG
// |||| ||||
// |||| |||+- greyscale
// |||| ||+-- show_bg_left
// |||| |+--- show_sprites_left
// |||| +---- show_background
// |||+------ show_sprites
// ||+------- emphasize_red
// |+-------- emphasize_green
// +--------- emphasize_blue
const PPUMask = packed struct {
    grayscale: u1 = 0, // bit 0
    show_background_left: u1 = 0, // bit 1
    show_sprites_left: u1 = 0, // bit 2
    show_background: u1 = 0, // bit 3
    show_sprites: u1 = 0, // bit 4
    emphasize_red: u1 = 0, // bit 5
    emphasize_green: u1 = 0, // bit 6
    emphasize_blue: u1 = 0, // bit 7
};
