const std = @import("std");

const PPU = @import("ppu.zig").PPU;
const Cartridge = @import("cartridge.zig").Cartridge;

// Bus is the interface responsible for communication between CPU and other components.
// Implementations must implement read and write methods.
pub const Bus = union(enum) {
    cpuTestBus: CPUTestBus,
    nesBus: NESBus,

    pub fn read(self: *Bus, addr: u16) u8 {
        // When we switch a tagged union, the captured value (eg |*v|) has the correct
        // type.
        switch (self.*) {
            .cpuTestBus => |*v| return v.read(addr),
            .nesBus => |*v| return v.read(addr),
        }
    }

    pub fn write(self: *Bus, addr: u16, val: u8) void {
        switch (self.*) {
            .cpuTestBus => |*v| v.write(addr, val),
            .nesBus => |*v| v.write(addr, val),
        }
    }
};

// CPUTestBus is the Bus implementation used for CPU testing. It contains a single
// array of bytes for memory.
pub const CPUTestBus = struct {
    mem: []u8,

    pub fn read(self: *CPUTestBus, addr: u16) u8 {
        return self.mem[addr];
    }

    pub fn write(self: *CPUTestBus, addr: u16, data: u8) void {
        self.mem[addr] = data;
    }

    pub fn init(mem: []u8) CPUTestBus {
        return CPUTestBus{
            .mem = mem,
        };
    }
};

pub const NESBus = struct {
    ram: []u8,
    ppu: *PPU,
    cart: *Cartridge, // TODO: will be used to determine mapper

    pub fn init(
        ram: []u8,
        ppu: *PPU,
        cartridge: *Cartridge,
    ) NESBus {
        return NESBus{
            .ram = ram,
            .ppu = ppu,
            .cart = cartridge,
        };
    }

    pub fn read(self: *NESBus, addr: u16) u8 {
        return switch (addr) {
            0x0000...0x1FFF => self.ram[addr & 0x07FF], // Mirror RAM every 2KB
            // Use mirroring to reduce the large address space into just the 8 entries
            // we need.
            0x2000...0x3FFF => self.ppu.readRegister(addr & 0x0007),
            0x4000...0x401F => 0, // TODO: APU & I/O
            0x4020...0x5FFF => 0, // TODO: Cartridge expansion ROM
            0x6000...0x7FFF => 0, // TODO: SRAM (battery-backed)
            0x8000...0xFFFF => {
                // PRG ROM mapping (mapper 0 NROM)
                // For NROM: if PRG is 16KB (1 bank), mirror it to both 0x8000 and 0xC000
                // if PRG is 32KB (2 banks), map them sequentially
                const prg_addr = addr - 0x8000;
                const prg_size = self.cart.prg_rom.len;
                return self.cart.prg_rom[prg_addr % prg_size];
            },
        };
    }

    pub fn write(self: *NESBus, addr: u16, data: u8) void {
        switch (addr) {
            0x0000...0x1FFF => self.ram[addr & 0x07FF] = data, // Mirror RAM every 2KB
            // Use mirroring to reduce the large address space into just the 8 entries
            // we need.
            0x2000...0x3FFF => self.ppu.writeRegister(addr & 0x0007, data),
            0x4000...0x401F => {}, // TODO: APU & I/O
            0x4020...0x5FFF => {}, // TODO: Cartridge expansion ROM
            0x6000...0x7FFF => {}, // TODO: SRAM (battery-backed)
            0x8000...0xFFFF => {}, // PRG ROM is read-only
        }
    }
};
