const Cartridge = @import("cartridge.zig").Cartridge;

pub const Mirroring = enum { horizontal, vertical };

// https://www.nesdev.org/wiki/UxROM
//
// Mapper 2. Two 16kb PRG windows in CPU address space:
// $8000-$BFFF: switchable bank, selected by bank register
// $C000-$FFFF: fixed to the last 16kb of PRG-ROM ("fixed to the last bank")
//
// Any write to $8000-$FFFF sets the bank register (low nibble), CHR is always 8kb
// CHR-RAM; mirroring is fixed by solder pad config (same as NROM)
pub const UxROM = struct {
    prg_bank: u8 = 0,
    bank_size: usize = 16 * 1024,

    pub fn cpuRead(self: UxROM, cart: *const Cartridge, addr: u16) u8 {
        const num_banks = cart.prg_rom.len / self.bank_size;

        // $8000-$BFFF = switchable bank, $C000-$FFFF = last bank
        const bank: usize = if (addr < 0xC000) self.prg_bank else num_banks - 1;
        const offset = (addr - 0x8000) & 0x3FFF; // offset within the 16kb window
        return cart.prg_rom[bank * self.bank_size + offset];
    }

    pub fn cpuWrite(self: *UxROM, cart: *const Cartridge, _: u16, val: u8) void {
        const num_banks = cart.prg_rom.len / self.bank_size;
        self.prg_bank = @intCast(val % num_banks);
    }

    pub fn chrRead(_: UxROM, cart: *const Cartridge, addr: u13) u8 {
        return cart.chr_rom[addr];
    }

    pub fn chrWrite(_: *UxROM, cart: *Cartridge, addr: u13, val: u8) void {
        cart.chr_rom[addr] = val; // CHR-RAM is writable
    }
};

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
    uxrom: UxROM,

    pub fn init(mapper_number: u8) !Mapper {
        return switch (mapper_number) {
            0 => .{ .nrom = .{} },
            2 => .{ .uxrom = .{} },
            else => error.UnsupportedMapper,
        };
    }
};
