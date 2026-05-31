const std = @import("std");
const Cartridge = @import("cartridge.zig").Cartridge;

pub const VramAddr = packed struct {
    coarse_x: u5 = 0, // bits 0–4
    coarse_y: u5 = 0, // bits 5–9
    // nametable holds 0-3, representing the four nametables to pick from
    nametable: u2 = 0, // bits 10–11
    fine_y: u3 = 0, // bits 12–14
    comptime {
        std.debug.assert(@bitSizeOf(VramAddr) == 15);
    }
};

// See Coarse X Increment
// https://www.nesdev.org/wiki/PPU_scrolling#At_dot_257_of_each_scanline
fn incrementCoarseX(v: *VramAddr) void {
    if (v.coarse_x == 31) {
        v.coarse_x = 0;
        // bounce between left and right nametables; 0b00 -> 0b01 (left->right), 0b01
        // -> 0b00 (right -> left), etc..
        v.nametable ^= 0b01;
    } else {
        v.coarse_x += 1;
    }
}

// See Coarse Y increment
// https://www.nesdev.org/wiki/PPU_scrolling#At_dot_257_of_each_scanline
fn incrementY(v: *VramAddr) void {
    if (v.fine_y < 7) {
        v.fine_y += 1;
    } else {
        v.fine_y = 0;
        if (v.coarse_y == 29) {
            v.coarse_y = 0;
            v.nametable ^= 0b10;
        } else if (v.coarse_y == 31) {
            v.coarse_y = 0;
        } else {
            v.coarse_y += 1;
        }
    }
}

fn resetHorizontal(v: *VramAddr, t: VramAddr) void {
    v.coarse_x = t.coarse_x;
    // copy horizontal (0b10) and preserve vertical (0b01)
    v.nametable = (v.nametable & 0b10) | (t.nametable & 0b01);
}

fn resetVertical(v: *VramAddr, t: VramAddr) void {
    v.coarse_y = t.coarse_y;
    v.fine_y = t.fine_y;
    // preserve horizontal (& 0b01) and copy vertical (0b10)
    v.nametable = (v.nametable & 0b01) | (t.nametable & 0b10);
}

pub const PPU = struct {
    ppu_ctrl: PPUCTRL = .{},
    ppu_mask: PPUMask = .{},
    ppu_status: PPUStatus = .{}, // $2002 read

    // PPU Memory
    vram: [2048]u8 = [_]u8{0} ** 2048, // 2KB internal VRAM
    // 32 bytes of palette RAM. First 16 for backgrounds, last 16 for sprites.
    // These 16 background bytes are four groups of four:
    //
    //   Index:   0  1  2  3    4  5  6  7    8  9 10 11   12 13 14 15
    //           ├─────────┤   ├─────────┤   ├─────────┤   ├─────────┤
    //            palette 0     palette 1     palette 2     palette 3
    //
    // Each slot holds a number from 0-63. That number is an index into the NES palette, a
    // fixed lookup table baked into PPU hardware.
    //
    // So if palette[7] contains 0x16, that means: "palette 1, colour 3 is NES system
    // colour 0x16". We have two numbers: palette index identifing which palette group.
    // And colour_val (0-3) which colour within that group.
    //
    // So, to get the right slot, we do array indexing:
    //
    // slot = palette_index * 4 + color_val (*4 because each palette is 4 entries wide)
    palette: [32]u8 = [_]u8{0} ** 32,
    oam: [256]u8 = [_]u8{0} ** 256,

    // Loopy registers. These render the background and support scrolling
    v: VramAddr = .{}, // $2006 write
    t: VramAddr = .{}, // temporary register for $2006 writes
    x: u3 = 0, // fine X scroll (3 bits)
    w: u1 = 0, // Write latch (0 = first write, 1 = second write) for $2006

    // OAM Registers & Memory
    oam_addr: u8 = 0, // $2003 write
    data_buffer: u8 = 0, // Used for delayed reads from VRAM

    cart: *Cartridge,

    // Tracking background CHR pattern bits
    bg_opaque: [256 * 240]bool = [_]bool{false} ** (256 * 240),

    cycle: u16 = 0,
    scanline: u16 = 0,
    nmi_triggered: bool = false,

    // read a byte from PPU memory
    fn ppuRead(self: *PPU, addr: u16) u8 {
        const mapped = addr & 0x3fff; // Mirror addresses above $3FFF down to $0000–$3FFF
        return switch (mapped) {
            // Pattern tables, chr-rom from the cartridge
            0x0000...0x1fff => {
                return self.cart.chrRead(@intCast(mapped));
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
            0x0000...0x1fff => self.cart.chrWrite(@intCast(mapped), val),
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
        const is_vertical = self.cart.mirroring() == .vertical;

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
    pub fn render(self: *PPU, buffer: []u8) void {
        if (self.cart.chr_rom.len == 0) return;

        self.renderBackground(buffer);
        self.renderSprites(buffer);
    }

    // reads the 32x30 tile grid from the active nametable, looks up CHR-ROM pattern
    // data, and outputs grayscale pixels.
    // ref: https://www.nesdev.org/wiki/PPU_nametables
    // ref: https://www.nesdev.org/wiki/PPU_pattern_tables
    //
    // nametable concepts: this is like a ping-pong buffer. While player sees nametable 1
    // scrolling onto screen, game is writing new tile data into nametable 0 (which is now
    // offscreen). When coarse_x wraps past nametable 1's edge, it toggles back to
    // nametable 0 (now with content). This is incrementCoarseX
    //
    // Each tile is 8x8 pixels, coarse_y tells us which tile row we're on. fine_y tells us
    // which pixel row within that tile (0-7)
    fn renderBackground(self: *PPU, buffer: []u8) void {
        @memset(buffer, 0);

        if (self.cart.chr_rom.len == 0) return;

        var v = self.v; // local snapshot to not stomp on emulation thread

        const bg_table: u16 = @as(u16, self.ppu_ctrl.background_pattern) * 0x1000;

        resetVertical(&v, self.t);

        for (0..240) |screen_y| {
            for (0..32) |screen_x| {
                // nametable address. PPU memory map has four nametables starting at
                // 0x2000. 0x400 is 1024, which is the size of each nametable. So 0x200 +
                // nametable * 0x400 gets us the base address of nametable 0, 1, 2, or 3.
                const nt_addr: u16 = 0x2000 +
                    @as(u16, v.nametable) * 0x400 +
                    @as(u16, v.coarse_y) * 32 +
                    @as(u16, v.coarse_x);
                const tile_index: u16 = self.vram[self.mirrorVramAddr(nt_addr)];
                const pattern_addr = bg_table + tile_index * 16;

                // Calculate which attribute byte covers this tile.
                // Each attribute byte covers a 4*4 tile region. The attribute table is 8
                // bytes wide (8 * 4 = 32 tiles = screen width). So for any tile at
                // (tile_x, tile_y), index = (y/4) * 8 + (x/4)
                const attr_addr: u16 = 0x2000 +
                    @as(u16, v.nametable) * 0x400 + 0x3c0 +
                    (@as(u16, v.coarse_y) / 4) * 8 +
                    (@as(u16, v.coarse_x) / 4);
                const attr_byte = self.vram[self.mirrorVramAddr(attr_addr)];

                // Then, quadrant shift. Each attribute byte packs 4 palette selections (2
                // bits each) for four 2*2-tile quadrants. We need to know which quadrant
                // the tile falls in. Division gives us the 2-tile column and then
                // identify odd/even placement with bitwise & 1
                const quadrant_x = (@as(u8, v.coarse_x) / 2) & 1;
                const quadrant_y = (@as(u8, v.coarse_y) / 2) & 1;
                const pshift: u3 = @intCast((quadrant_y * 2 + quadrant_x) * 2);
                const palette_index: u8 = (attr_byte >> pshift) & 0x03;

                // fine_y picks the row within the tile
                const row = @as(u16, v.fine_y);
                const low_byte = self.cart.chrRead(@intCast(pattern_addr + row));
                const high_byte = self.cart.chrRead(@intCast(pattern_addr + row + 8));

                for (0..8) |col| {
                    const shift: u3 = @intCast(7 - col);
                    const bit0 = (low_byte >> shift) & 1;
                    const bit1 = (high_byte >> shift) & 1;
                    const color_val: u8 = (bit1 << 1) | bit0;

                    const pixel_x = screen_x * 8 + col;
                    const offset = (screen_y * 256 + pixel_x) * 4;

                    // We must record the opacity of the background bit, so we can use as
                    // part of sprite priority when rendering sprites.
                    self.bg_opaque[screen_y * 256 + pixel_x] = (color_val != 0);

                    const rgb: [3]u8 = if (color_val == 0)
                        nes_palette[self.palette[0] & 0x3F]
                    else
                        nes_palette[self.palette[palette_index * 4 + color_val] & 0x3f];

                    buffer[offset + 0] = rgb[0]; // R
                    buffer[offset + 1] = rgb[1]; // G
                    buffer[offset + 2] = rgb[2]; // B
                    buffer[offset + 3] = 0xFF; // A
                }
                incrementCoarseX(&v);
            }
            incrementY(&v); // after each pixel row, advance Y
            resetHorizontal(&v, self.t);
        }
    }

    fn renderSprites(self: *PPU, buffer: []u8) void {
        if (self.cart.chr_rom.len == 0) return;

        // PPUCTRL has the bit which selects which pattern table to use. Pattern table 0
        // begins at $0000, table 1 at $1000. Multiplication via 0x1000 converts the 0 / 1
        // bit to base address.
        const sprite_table: u16 = @as(u16, self.ppu_ctrl.sprite_pattern) * 0x1000;

        for (0..64) |i| { // OAM is 256 bytes and each sprite is 4 = 64 sprites
            const base = i * 4; // each sprite is 4 bytes
            const y: u16 = @as(u16, self.oam[base]) + 1; // + 1 is hardware quirk
            const tile: u16 = self.oam[base + 1]; // which 8x8 pattern to draw from CHRROM
            const attrs = self.oam[base + 2];
            // 76543210
            // ||||||||
            // ||||||++- Palette (4 to 7) of sprite
            // |||+++--- Unimplemented (read 0)
            // ||+------ Priority (0: in front of background; 1: behind background)
            // |+------- Flip sprite horizontally
            // +-------- Flip sprite vertically
            const sprite_palette: u8 = (attrs & 0x03) + 4; // palettes 4-7
            const flip_h = (attrs & 0x40) != 0;
            const flip_v = (attrs & 0x80) != 0;
            const behind_bg = (attrs & 0x20) != 0;
            const x: u16 = self.oam[base + 3];

            if (y >= 240) continue; // hide anything off screen

            const pattern_addr = sprite_table + tile * 16;

            for (0..8) |row| {
                // Same as background: Each row of a tile is two bytes, lo/hi bitplane
                // seperated by 8 bytes
                const actual_row = if (flip_v) 7 - row else row;
                const low_byte = self.cart.chrRead(@intCast(pattern_addr + actual_row));
                const high_byte = self.cart.chrRead(@intCast(pattern_addr + actual_row + 8));

                for (0..8) |col| {
                    const pixel_x = x + col;
                    const pixel_y = y + row;
                    if (pixel_x >= 256 or pixel_y >= 240) continue;

                    // Extract 2-bit colour value for pixel.
                    const shift: u3 = @intCast(if (flip_h) col else 7 - col);
                    const bit0 = (low_byte >> shift) & 1;
                    const bit1 = (high_byte >> shift) & 1;
                    const color_val: u8 = (bit1 << 1) | bit0;

                    if (color_val == 0) continue; // transparent
                    // And also skip if behind background
                    if (behind_bg and self.bg_opaque[pixel_y * 256 + pixel_x]) continue;

                    const offset = (pixel_y * 256 + pixel_x) * 4;
                    const rgb: [3]u8 = nes_palette[self.palette[sprite_palette * 4 + color_val] & 0x3F];
                    buffer[offset + 0] = rgb[0];
                    buffer[offset + 1] = rgb[1];
                    buffer[offset + 2] = rgb[2];
                    buffer[offset + 3] = 0xFF;
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
