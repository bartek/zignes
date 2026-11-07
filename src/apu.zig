const std = @import("std");

// Each Pulse channel contains the following:
// enevelope generator, sweep unit, timer, 8-step sequencer, length counter
// https://www.nesdev.org/wiki/APU_Pulse
//              Sweep -----> Timer
//                       |            |
//                       |            |
//                       |            v
//                       |        Sequencer   Length Counter
//                       |            |             |
//                       |            |             |
//                       v            v             v
//    Envelope -------> Gate -----> Gate -------> Gate --->(to mixer)
//
// Registers are represented through packed struct types, see below (e.g. PulseControl,
// PulseSweep)
const Pulse = struct {
    control: PulseControl = .{},
    sweep: PulseSweep = .{},
    timer_period: u11 = 0,

    // write updates the register in Pulse. Expects a truncated address so only low 2 bits
    // of the address are switched on.
    pub fn write(self: *Pulse, register: u2, val: u8) void {
        switch (register) {
            0 => self.control = @bitCast(val),
            1 => self.sweep = @bitCast(val),
            // set the low 8 bits of timer period, keep the high 3
            2 => self.timer_period = (self.timer_period & 0x700) | val,
            3 => {
                const load: PulseLoad = @bitCast(val);
                self.timer_period = (@as(u11, load.timer_hi) << 8) | (self.timer_period & 0xFF);
            },
        }
    }
};

// $4000 / $4004 :: DDLC VVVV
const PulseControl = packed struct(u8) {
    volume: u4 = 0, // V: volume / envelope divider period.
    constant_volume: bool = false, // C: constant volume
    length_halt: bool = false, // L: length counter halt
    duty: u2 = 0, // D: duty cycle

};

// https://www.nesdev.org/wiki/APU_Sweep
const PulseSweep = packed struct(u8) {
    shift: u3 = 0,
    negate: bool = false,
    period: u3 = 0,
    enabled: bool = false,
};

// $4003 / $4007 :: LLLL LTTT. Length counter load index + high 3 bits of the 11 bit timer.
// ($4002 / $4006 is the plain low byte of the timer, no struct needed.)
const PulseLoad = packed struct(u8) {
    timer_hi: u3,
    length_index: u5,
};

pub const APU = struct {
    // The APU has five channels: two pulse waves, triangle wave, noise, and DMC (sample
    // playback). DMC is not covered in "basics"
    pulse1: Pulse = .{},
    pulse2: Pulse = .{},
    irq_pending: bool = false,

    pub fn writeRegister(self: *APU, addr: u16, val: u8) void {
        switch (addr) {
            0x4000...0x4003 => self.pulse1.write(@truncate(addr), val),
            0x4004...0x4007 => self.pulse2.write(@truncate(addr), val),
            else => {},
        }
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

test "APU default-initializes" {
    // `zig test` only analyzes what tests reference, so without this a broken
    // default (e.g. a missing `= .{}`) only shows up in `zig build`.
    const apu: APU = .{};
    _ = apu;
}

test "PulseControl matches DDLC VVV" {
    // Mostly verify the packed struct is behaving as we need.
    const ctrl: PulseControl = @bitCast(@as(u8, 0b10_0_1_0101));
    try std.testing.expectEqual(2, ctrl.duty); // DD = 10
    try std.testing.expectEqual(false, ctrl.length_halt); // L = 0
    try std.testing.expectEqual(true, ctrl.constant_volume); // C = 1
    try std.testing.expectEqual(5, ctrl.volume); // VVVV = 0101
}

test "writing $4000-$4003 configures pulse 1" {
    var apu: APU = .{};
    apu.writeRegister(0x4000, 0b10_0_1_0101);
    apu.writeRegister(0x4002, 0xFD); // timer low = 0xFD
    apu.writeRegister(0x4003, 0b00000_000); // timer high = 0 -> period 253

    try std.testing.expectEqual(2, apu.pulse1.control.duty);
    try std.testing.expectEqual(5, apu.pulse1.control.volume);
    try std.testing.expectEqual(253, apu.pulse1.timer_period);
}

test "writing $4004-$4007 configures pulse 2" {
    var apu: APU = .{};
    apu.writeRegister(0x4004, 0b10_0_1_0101);
    apu.writeRegister(0x4006, 0xFD); // timer low = 0xFD
    apu.writeRegister(0x4007, 0b00000_000); // timer high = 0 -> period 253

    try std.testing.expectEqual(2, apu.pulse2.control.duty);
    try std.testing.expectEqual(5, apu.pulse2.control.volume);
    try std.testing.expectEqual(253, apu.pulse2.timer_period);
    try std.testing.expectEqual(0, apu.pulse1.timer_period); // pulse 1 untouched
}
