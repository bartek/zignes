const std = @import("std");

pub const PPU = struct {
    ppu_ctrl: PPUCTRL = .{},
    ppu_mask: PPUMask = .{},

    pub fn readRegister(self: *PPU, addr: u16) u8 {
        std.debug.assert(addr >= 0 and addr <= 8);
        _ = self;
        return 0;
    }

    pub fn writeRegister(self: *PPU, addr: u16, val: u8) void {
        // We write to addres space $2000 - $4014
        // https://www.nesdev.org/wiki/PPU_registers
        std.debug.assert(addr >= 0x2000 and addr <= 0x4014);

        // handle 0-7 by masking
        const register = addr & 0x0007;

        switch (register) {
            0 => { // PPUCTRL
                self.ppu_ctrl = @bitCast(val);
            },
            1 => self.ppu_mask = @bitCast(val), // PPUMASK
            2 => return, // PPUSTATUS is read only
            else => unreachable,
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
