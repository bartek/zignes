const std = @import("std");

pub const PPU = struct {
    ppu_ctrl: PPUCTRL = .{},
    ppu_mask: PPUMask = .{},

    // OAM Registers & Memory
    oam_addr: u8 = 0, // $2003 write

    cycle: u16 = 0,
    scanline: u16 = 0,

    pub fn readRegister(self: *PPU, addr: u16) u8 {
        std.debug.assert(addr >= 0 and addr <= 8);

        return switch (addr) {
            0...8 => {
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
            },
            1 => self.ppu_mask = @bitCast(val), // PPUMASK
            2 => return, // PPUSTATUS is read only
            3 => self.oam_addr = val,
            4...8 => {
                // TODO
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
                self.scanline = -1;
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
