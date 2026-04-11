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
    ppu_status: PPUStatus = .{}, // $2002 read

    // PPU Memory
    vram: [2048]u8 = [_]u8{0} ** 2048, // 2KB internal VRAM
    palette: [32]u8 = [_]u8{0} ** 32, // 32 bytes of palette RAM
    oam: [256]u8 = [_]u8{0} ** 256,

    // Loopy registers. These render the background and support scrolling
    v: VramAddr = .{}, // $2006 write
    t: VramAddr = .{}, // temporary register for $2006 writes
    x: u3 = 0, // fine X scroll (3 bits)
    w: u1 = 0, // Write latch (0 = first write, 1 = second write) for $2006

    // OAM Registers & Memory
    oam_addr: u8 = 0, // $2003 write
    data_buffer: u8 = 0, // Used for delayed reads from VRAM

    cart: ?*const Cartridge = null,

    cycle: u16 = 0,
    scanline: u16 = 0,
    nmi_triggered: bool = false,

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
                const status: u8 = @bitCast(self.ppu_status);
                // Reading status clears the vblank flag
                self.ppu_status.vblank_started = 0;
                // Reset address latch
                self.w = 0;
                return status;
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
                const was_nmi_off = self.ppu_ctrl.nmi_enable == 0;
                self.ppu_ctrl = @bitCast(val);
                // Also update the nametable bits in the temporary register `t`
                self.t.nametable = self.ppu_ctrl.nametable;
                // Hardware quirk: enabling NMI while already in vblank triggers immediately
                if (was_nmi_off and self.ppu_ctrl.nmi_enable == 1 and self.ppu_status.vblank_started == 1) {
                    self.nmi_triggered = true;
                }
            },
            1 => self.ppu_mask = @bitCast(val), // PPUMASK
            2 => return, // PPUSTATUS is read only
            3 => self.oam_addr = val, // OAMADDR
            5 => { // PPUSCROLL
                if (self.w == 0) {
                    // First write: fine X and coarse X
                    self.t.coarse_x = @intCast(val >> 3);
                    self.x = @intCast(val & 0x07);
                    self.w = 1;
                } else {
                    // Second write: fine Y and coarse Y
                    self.t.fine_y = @intCast(val & 0x07);
                    self.t.coarse_y = @intCast(val >> 3);
                    self.w = 0;
                }
            },
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
            4, 8 => {
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

            // Called at the start of vblank (scanline 241, dot 1)
            if (self.scanline == 241) {
                self.ppu_status.vblank_started = 1;
                if (self.ppu_ctrl.nmi_enable == 1) {
                    self.nmi_triggered = true;
                }
            }

            if (self.scanline >= 261) {
                self.scanline = 0;
                self.ppu_status.vblank_started = 0;
                self.ppu_status.sprite_zero_hit = 0;
                self.ppu_status.sprite_overflow = 0;
            }
        }
    }

    // Render PPU framebuffer to pixel data
    // Returns a buffer of (256 * 240 * 4) bytes in RGBA format
    pub fn render(self: *const PPU, buffer: []u8) void {
        self.renderBackground(buffer);
    }

    // reads the 32x30 title grid from the active nametable, looks up CHR-ROM pattern
    // data, and outputs grayscale pixels.
    fn renderBackground(self: *const PPU, buffer: []u8) void {
        @memset(buffer, 0);

        const cart = self.cart orelse return;
        const chr = cart.chr_rom;
        if (chr.len == 0) return;

        const bg_table: u16 = @as(u16, self.ppu_ctrl.background_pattern) * 0x1000;
        const nt_base: u16 = 0x2000 + @as(u16, self.ppu_ctrl.nametable) * 0x400;

        for (0..30) |tile_y| {
            for (0..32) |tile_x| {
                const nt_addr = nt_base + @as(u16, @intCast(tile_y)) * 32 + @as(u16, @intCast(tile_x));
                const tile_index: u16 = self.vram[self.mirrorVramAddr(nt_addr)];
                const pattern_addr = bg_table + tile_index * 16;

                for (0..8) |row| {
                    const low_byte = chr[pattern_addr + row];
                    const high_byte = chr[pattern_addr + row + 8];

                    for (0..8) |col| {
                        const shift: u3 = @intCast(7 - col);
                        const bit0 = (low_byte >> shift) & 1;
                        const bit1 = (high_byte >> shift) & 1;
                        const color_val: u8 = (bit1 << 1) | bit0;

                        const pixel_x = tile_x * 8 + col;
                        const pixel_y = tile_y * 8 + row;
                        const offset = (pixel_y * 256 + pixel_x) * 4;

                        // Use palette[0] for background, grays for 1-3
                        const rgb: [3]u8 = if (color_val == 0)
                            nes_palette[self.palette[0] & 0x3F]
                        else switch (color_val) {
                            1 => .{ 0x6A, 0x6A, 0x6A },
                            2 => .{ 0xAA, 0xAA, 0xAA },
                            3 => .{ 0xFF, 0xFF, 0xFF },
                            else => unreachable,
                        };

                        buffer[offset + 0] = rgb[0]; // R
                        buffer[offset + 1] = rgb[1]; // G
                        buffer[offset + 2] = rgb[2]; // B
                        buffer[offset + 3] = 0xFF; // A
                    }
                }
            }
        }
    }
};

// 2C02 PPU system palette: maps 6-bit NES color index to RGB.
// ref: https://www.nesdev.org/wiki/PPU_palettes
const nes_palette = [64][3]u8{
    .{ 0x62, 0x62, 0x62 }, .{ 0x00, 0x2E, 0x98 }, .{ 0x15, 0x14, 0xA5 }, .{ 0x35, 0x00, 0x93 },
    .{ 0x4C, 0x00, 0x72 }, .{ 0x56, 0x00, 0x3E }, .{ 0x52, 0x05, 0x00 }, .{ 0x3F, 0x18, 0x00 },
    .{ 0x22, 0x2B, 0x00 }, .{ 0x05, 0x39, 0x00 }, .{ 0x00, 0x3C, 0x00 }, .{ 0x00, 0x35, 0x22 },
    .{ 0x00, 0x2A, 0x66 }, .{ 0x00, 0x00, 0x00 }, .{ 0x00, 0x00, 0x00 }, .{ 0x00, 0x00, 0x00 },
    .{ 0xAB, 0xAB, 0xAB }, .{ 0x0F, 0x63, 0xDE }, .{ 0x37, 0x40, 0xFE }, .{ 0x6B, 0x25, 0xFE },
    .{ 0x90, 0x15, 0xC8 }, .{ 0x9E, 0x15, 0x80 }, .{ 0x98, 0x23, 0x2A }, .{ 0x80, 0x3D, 0x00 },
    .{ 0x5B, 0x56, 0x00 }, .{ 0x30, 0x6A, 0x00 }, .{ 0x10, 0x70, 0x00 }, .{ 0x00, 0x68, 0x42 },
    .{ 0x00, 0x5C, 0x9E }, .{ 0x00, 0x00, 0x00 }, .{ 0x00, 0x00, 0x00 }, .{ 0x00, 0x00, 0x00 },
    .{ 0xFF, 0xFF, 0xFF }, .{ 0x53, 0xAE, 0xFF }, .{ 0x70, 0x8E, 0xFF }, .{ 0xA2, 0x7B, 0xFF },
    .{ 0xDE, 0x6C, 0xFF }, .{ 0xEF, 0x6D, 0xC4 }, .{ 0xF0, 0x7A, 0x69 }, .{ 0xD5, 0x92, 0x16 },
    .{ 0xAC, 0xA9, 0x00 }, .{ 0x7F, 0xBC, 0x00 }, .{ 0x5A, 0xC5, 0x13 }, .{ 0x42, 0xBF, 0x62 },
    .{ 0x47, 0xB4, 0xBB }, .{ 0x4A, 0x4A, 0x4A }, .{ 0x00, 0x00, 0x00 }, .{ 0x00, 0x00, 0x00 },
    .{ 0xFF, 0xFF, 0xFF }, .{ 0xB6, 0xDA, 0xFF }, .{ 0xC5, 0xCA, 0xFF }, .{ 0xDA, 0xC2, 0xFF },
    .{ 0xF0, 0xBE, 0xFF }, .{ 0xF8, 0xBF, 0xE4 }, .{ 0xF8, 0xC5, 0xBB }, .{ 0xEE, 0xCF, 0x9C },
    .{ 0xDA, 0xDA, 0x8B }, .{ 0xC4, 0xE3, 0x8B }, .{ 0xB2, 0xE8, 0x97 }, .{ 0xA6, 0xE5, 0xB4 },
    .{ 0xA8, 0xDF, 0xDB }, .{ 0xB0, 0xB0, 0xB0 }, .{ 0x00, 0x00, 0x00 }, .{ 0x00, 0x00, 0x00 },
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

// 7  bit  0
// ---- ----
// VSO. ....
// |||| ||||
// |||+-++++- PPU open bus. Returns stale PPU bus contents.
// ||+------- Sprite overflow.
// |+-------- Sprite 0 Hit.
// +--------- Vertical blank has started.
const PPUStatus = packed struct {
    open_bus: u5 = 0, // bits 0-4
    sprite_overflow: u1 = 0, // bit 5
    sprite_zero_hit: u1 = 0, // bit 6
    vblank_started: u1 = 0, // bit 7
};
