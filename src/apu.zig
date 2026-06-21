const std = @import("std");

pub const APU = struct {
    irq_pending: bool = false,

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
