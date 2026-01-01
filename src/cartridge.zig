const std = @import("std");

const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

// 76543210
// ||||||||
// |||||||+- Nametable arrangement: 0: vertical arrangement ("horizontal mirrored") (CIRAM A10 = PPU A11)
// |||||||                          1: horizontal arrangement ("vertically mirrored") (CIRAM A10 = PPU A10)
// ||||||+-- 1: Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
// |||||+--- 1: 512-byte trainer at $7000-$71FF (stored before PRG data)
// ||||+---- 1: Alternative nametable layout
// ++++----- Lower nybble of mapper number
pub const Flags6 = packed struct(u8) {
    // 0 = horizontal, 1 = vertical
    mirroring_is_vertical: bool = false,
    // battery-backed PRG RAM at $6000-$7FFF
    has_prg_ram: bool = false,
    // 512-byte trainer at $7000-$71FF
    has_trainer: bool = false,
    // alternative nametable layout
    alternative_nametable: bool = false,
    // lower nibble of mapper number
    lower_mapper_nibble: u4 = 0,
};

// 76543210
// ||||||||
// |||||||+- VS Unisystem
// ||||||+-- PlayChoice-10 (8 KB of Hint Screen data stored after CHR data)
// ||||++--- If equal to 2, flags 8-15 are in NES 2.0 format
// ++++----- Upper nybble of mapper number
pub const Flags7 = packed struct(u8) {
    // console type set to Nintendo VS Unisystem
    console_type: u2 = 0b00,

    // identifier is always set to "2" on init
    nes_identifier: u2 = 0b00,

    // upper nibble of mapper number
    upper_mapper_nibble: u4 = 0,
};

// Header is an NES ROM header in INES format (defacto standard for distribution of NES
// binary programs)
// https://www.nesdev.org/wiki/INES
pub const Header = extern struct {

    // The format of the header is as follows:
    // Bytes 0-3, Constant $4E $45 $53 $1A (ASCII "NES" followed by MS-DOS end-of-file)
    pub const Magic = packed struct {
        N: u8 = 0,
        E: u8 = 0,
        S: u8 = 0,
        EOF: u8 = 0,
        comptime {
            assert(@sizeOf(Magic) == 4);
        }
    };

    NES: Magic = .{},

    // Byte 4, Size of PRG ROM in 16 KB units
    prg_rom_size: u8,

    // Byte 5, Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)
    chr_rom_size: u8,

    // Byte 6, Flags 6 (mapper, mirroring, battery, trainer)
    flags_6: Flags6 = .{},

    // Byte 7, Flags 7 (mapper, VS/Playchoice, NES 2.0)
    flags_7: Flags7 = .{},

    // A value of 0 assumes 8KB of PRG RAM for compatibility.
    // rarely used and ignored for implementation.
    prg_ram_size: u8 = 0,

    pub fn isValid(self: Header) bool {
        return self.NES.N == 'N' and
            self.NES.E == 'E' and
            self.NES.S == 'S' and
            self.NES.EOF == 0x1A;
    }
};

pub const Cartridge = struct {
    header: Header,
    prg_rom: []const u8,
    chr_rom: []const u8,
    allocator: Allocator,

    pub fn loadFromFile(allocator: Allocator, path: [*:0]const u8) !Cartridge {
        var file = try std.fs.cwd().openFileZ(path, .{});
        defer file.close();

        var buf = [_]u8{0} ** @sizeOf(Header);
        const bytes_read = try file.read(&buf);
        assert(bytes_read == @sizeOf(Header));

        const header: Header = @bitCast(buf);
        assert(header.isValid());

        // Skip trainer if present (512 bytes at $7000-$71FF)
        if (header.flags_6.has_trainer) {
            var trainer_buf: [512]u8 = undefined;
            const trainer_read = try file.read(&trainer_buf);
            assert(trainer_read == 512);
        }

        // Load PRG ROM (16 KB units)
        const prg_rom_size = @as(usize, header.prg_rom_size) * 16 * 1024;
        const prg_rom_buf = try allocator.alloc(u8, prg_rom_size);
        const prg_read = try file.read(prg_rom_buf);
        assert(prg_read == prg_rom_size);

        // Load CHR ROM (8 KB units)
        const chr_rom_size = @as(usize, header.chr_rom_size) * 8 * 1024;
        const chr_rom_buf = try allocator.alloc(u8, chr_rom_size);
        const chr_read = try file.read(chr_rom_buf);
        assert(chr_read == chr_rom_size);

        return .{
            .allocator = allocator,
            .header = header,
            .prg_rom = prg_rom_buf,
            .chr_rom = chr_rom_buf,
        };
    }

    pub fn deinit(self: *Cartridge) void {
        self.allocator.free(self.prg_rom);
        self.allocator.free(self.chr_rom);
    }
};

const T = std.testing;

test "Cartridge load and verify header" {
    var cart = try Cartridge.loadFromFile(T.allocator, "roms/donkeykong.nes");
    defer cart.deinit();
}
