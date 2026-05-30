const std = @import("std");

const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Mapper = @import("mappers.zig").Mapper;
const Mirroring = @import("mappers.zig").Mirroring;

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

    // Bytes 9-15: unused padding to fill out the 16-byte iNES header
    _padding: [7]u8 = [_]u8{0} ** 7,

    comptime {
        assert(@sizeOf(Header) == 16);
    }

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
    chr_rom: []u8,
    mapper: Mapper,
    allocator: Allocator,

    pub fn loadFromFile(allocator: Allocator, path: [*:0]const u8) !Cartridge {
        var file = try std.fs.cwd().openFileZ(path, .{});
        defer file.close();
        const bytes = try file.readToEndAlloc(allocator, 16 * 1024 * 1024); // cap at 16MB
        defer allocator.free(bytes);
        return load(allocator, bytes);
    }

    pub fn load(allocator: Allocator, bytes: []const u8) !Cartridge {
        if (bytes.len < @sizeOf(Header)) return error.RomTooSmall;

        const header: Header = @bitCast(bytes[0..@sizeOf(Header)].*);
        if (!header.isValid()) return error.InvalidHeader;

        var offset: usize = @sizeOf(Header);
        if (header.flags_6.has_trainer) offset += 512;

        const prg_rom_size = @as(usize, header.prg_rom_size) * 16 * 1024;

        const chr_rom_size = @as(usize, header.chr_rom_size) * 8 * 1024;
        const uses_chr_ram = chr_rom_size == 0;
        const chr_alloc_size: usize = if (uses_chr_ram) 8 * 1024 else chr_rom_size;

        // UxROM (and possibly other mappers not yet implementd) store chr_rom_size = 0 in
        // the iNES header, meaning: the cartridge has 8 KB of writable CHR-RAM, no CHR
        // data shipped.
        if (!uses_chr_ram and bytes.len < offset + prg_rom_size + chr_rom_size) {
            return error.RomTruncated;
        }

        const prg_rom_buf = try allocator.alloc(u8, prg_rom_size);
        @memcpy(prg_rom_buf, bytes[offset..][0..prg_rom_size]);
        offset += prg_rom_size;

        const chr_buf = try allocator.alloc(u8, chr_alloc_size);

        // When uses_chr_ram is false, means the iNES file contains graphics data
        // (typically 8KB or more of a pattern table). When false, we copy the bytes from
        // the file slice and put them into our owned chr_buf.
        //
        // When uses_chr_ram is true, it means the iNES file contains zero bytes of
        // graphics data. There's nothing to copy, so we fill the buffer with zero to
        // clean any random bytes the allocator may have placed on alloc.
        if (uses_chr_ram) {
            @memset(chr_buf, 0);
        } else {
            @memcpy(chr_buf, bytes[offset..][0..chr_rom_size]);
        }

        // Mapper number is split across two header bytes. Both are low nibbles (015),
        // shift the high left by 4 so they occupy the upper byte half.
        const mapper_num: u8 =
            (@as(u8, header.flags_7.upper_mapper_nibble) << 4) |
            @as(u8, header.flags_6.lower_mapper_nibble);

        return .{
            .allocator = allocator,
            .header = header,
            .prg_rom = prg_rom_buf,
            .chr_rom = chr_buf,
            .mapper = try Mapper.init(mapper_num),
        };
    }

    pub fn cpuRead(self: *const Cartridge, addr: u16) u8 {
        return switch (self.mapper) {
            inline else => |m| m.cpuRead(self, addr),
        };
    }

    pub fn cpuWrite(self: *Cartridge, addr: u16, val: u8) void {
        switch (self.mapper) {
            inline else => |*m| m.cpuWrite(self, addr, val),
        }
    }

    pub fn chrRead(self: *const Cartridge, addr: u13) u8 {
        return switch (self.mapper) {
            inline else => |m| m.chrRead(self, addr),
        };
    }
    pub fn chrWrite(self: *Cartridge, addr: u13, val: u8) void {
        switch (self.mapper) {
            inline else => |*m| m.chrWrite(self, addr, val),
        }
    }

    pub fn mirroring(self: *const Cartridge) Mirroring {
        return if (self.header.flags_6.mirroring_is_vertical) .vertical else .horizontal;
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
