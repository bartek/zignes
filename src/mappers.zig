const Cartridge = @import("cartridge.zig").Cartridge;

pub const Mirroring = enum { horizontal, vertical };

// NROM https://www.nesdev.org/wiki/NROM
pub const NROM = struct {
    // NROM has no state, no banks to switch. All banks are fixed. The cart's flat prg_rom
    // and chr_rom slices are the memory map.
    //
    pub fn cpuRead(_: NROM, cart: *const Cartridge, addr: u16) u8 {
        // For NROM: if PRG is 16KB (1 bank), mirror it to both 0x8000 and 0xC000
        // if PRG is 32KB (2 banks), map them sequentially
        const prg_addr = (addr - 0x8000) % cart.prg_rom.len;
        return cart.prg_rom[prg_addr];
    }

    pub fn cpuWrite(_: NROM, _: *const Cartridge, _: u16, _: u8) void {
        // Writes to $8000-$FFFF on NROM do nothing. ROM is read-only
    }

    pub fn chrRead(_: NROM, cart: *const Cartridge, addr: u13) u8 {
        return cart.chr_rom[addr];
    }

    pub fn chrWrite(_: *NROM, _: *Cartridge, _: u13, _: u8) void {
        // NROM uses CHR-ROM (read-only). Writes are no-op
    }
};

// The NES was deliberately cheap (1983 hardware, designed to lose to home computers on
// price). So, Nintendo offloaded the "make this game bigger" problem onto the cartridge:
// if you want more ROM, put login on your cart that fakes a larger memory space by
// swapping chunks in and out.
//
// That logic is the mapper!
//
// NROM is the "no mapper" mapper: no extra chip, just two ROM chips wired directly to the
// bus.
pub const Mapper = union(enum) {
    nrom: NROM,

    pub fn init(mapper_number: u8) !Mapper {
        return switch (mapper_number) {
            0 => .{ .nrom = .{} },
            else => error.UnsupportedMapper,
        };
    }
};
