const std = @import("std");
const Allocator = std.mem.Allocator;

pub const APU = struct {
    irq_pending: bool = false,
    allocator: Allocator,

    pub fn init(allocator: Allocator) APU {
        return APU{
            .allocator = allocator,
        };
    }

    pub fn writeRegister(self: *APU, addr: u16, val: u8) void {
        _ = self;
        _ = addr;
        _ = val;
    }

    pub fn readRegister(self: *APU, addr: u16) u8 {
        _ = self;
        _ = addr;
        return 0;
    }

    pub fn tick(self: *APU) void {
        _ = self;
    }

    pub fn sample() f32 {}
};
