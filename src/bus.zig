const std = @import("std");

const PPU = @import("ppu.zig").PPU;

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

    pub fn init(
        ram: []u8,
        ppu: *PPU,
    ) NESBus {
        return NESBus{
            .ram = ram,
            .ppu = ppu,
        };
    }

    pub fn read(self: *NESBus, addr: u16) u8 {
        return switch (addr) {
            0x0000...0x1FFF => self.ram[addr],
            // Use mirroring to reduce the large address space into just the 8 entries
            // we need.
            0x2000...0x3FFF => self.ppu.readRegister(addr & 0x0007),
            else => self.ram[addr],
        };
    }

    pub fn write(self: *NESBus, addr: u16, data: u8) void {
        switch (addr) {
            0x0000...0x1FFF => self.ram[addr] = data,
            // Use mirroring to reduce the large address space into just the 8 entries
            // we need.
            0x2000...0x3FFF => self.ppu.writeRegister(addr & 0x0007, data),
            else => self.ram[addr] = data,
        }
    }
};
