const std = @import("std");
const timing = @import("timing.zig");

// Host audio rate: how many samples per second we hand to the sound card.
pub const sample_rate: f64 = 44_100.0;

// How many CPU cycles pass between two output samples
const cycles_per_sample: f64 = timing.cpu_hz / sample_rate; // ~40.58

// Mixer lookup table.
// A 31-entry table for the two pulse channels
// https://www.nesdev.org/wiki/APU_Mixer (Lookup Table)
const pulse_lookup_table = blk: {
    var table: [31]f32 = undefined;
    table[0] = 0;
    for (1..table.len) |n| {
        table[n] = 95.52 / (8128.0 / @as(f32, @floatFromInt(n)) + 100.0);
    }
    break :blk table;
};

// Duty Cycle Sequences per
// https://www.nesdev.org/wiki/APU_Pulse#Sequencer_operation
const duty_cycle_sequences = [4][8]u1{
    .{ 0, 1, 0, 0, 0, 0, 0, 0 }, // 0: 12.5%
    .{ 0, 1, 1, 0, 0, 0, 0, 0 }, // 1: 25%
    .{ 0, 1, 1, 1, 1, 0, 0, 0 }, // 2: 50%
    .{ 1, 0, 0, 1, 1, 1, 1, 1 }, // 3  25% negated
};

const length_table = [32]u8{ 10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30 };

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

    // length counter
    length_counter: u8 = 0,
    counter_enabled: bool = false,

    // duty
    timer: u11 = 0, // current countdown
    sequence_step: u3 = 0, // index into duty_cycle_sequences

    // output returns, to the mixer, the pulse channel's current envelope or zero
    // https://www.nesdev.org/wiki/APU_Pulse#Mixer_output
    pub fn output(self: *const Pulse) u4 {
        // If any of the next conditions are true, pulse channel sends zero (silence) to
        // the mixer.

        // sequencer output is zero
        if (duty_cycle_sequences[self.control.duty][self.sequence_step] == 0) {
            return 0;
        }

        // the length counter is zero
        if (self.length_counter == 0) {
            return 0;
        }

        // the timer value is less than eight
        if (self.timer_period < 8) {
            return 0;
        }

        // TODO
        // overflow from the sweep unit's adder is silencing the channel
        return self.control.volume;
    }

    // clockTimer counts the timer. Each time it passes 0 it reloads and advances the
    // sequencer one step. Called once per APU cycle
    pub fn clockTimer(self: *Pulse) void {
        if (self.timer == 0) {
            self.timer = self.timer_period;
            self.sequence_step +%= 1; // u3 wraps 7 -> 0
        } else {
            self.timer -= 1;
        }
    }

    pub fn clockLength(self: *Pulse) void {
        if (self.control.length_halt) {
            // do nothing, hold note
        } else {
            if (self.length_counter > 0) {
                self.length_counter = self.length_counter - 1;
            }
        }
    }

    // write updates the register in Pulse. Expects a truncated address so only low 2 bits
    // of the address are switched on.
    pub fn write(self: *Pulse, register: u2, val: u8) void {
        switch (register) {
            0 => self.control = @bitCast(val),
            1 => self.sweep = @bitCast(val),
            // set the low 8 bits of timer period, keep the high 3
            2 => self.timer_period = (self.timer_period & 0x700) | val,
            3 => {
                // Length counter load value and timer period upper 3 bits.
                const load: PulseLoad = @bitCast(val);
                if (self.counter_enabled) {
                    self.length_counter = length_table[load.length_index];
                }
                self.timer_period = (@as(u11, load.timer_hi) << 8) | (self.timer_period & 0xFF);

                // Reset the sequencer index (i.e the pulse phase)
                self.sequence_step = 0;
            },
        }
    }

    // setCounter sets counter to on or off
    pub fn setCounter(self: *Pulse, on: bool) void {
        if (!on) {
            self.length_counter = 0;
        }
        self.counter_enabled = on;
    }
};

// ChannelEnable supports $4015 write
const ChannelEnable = packed struct(u8) {
    pulse1: bool = false, // 1
    pulse2: bool = false, // 2
    triangle: bool = false, // T
    noise: bool = false, // N
    dmc: bool = false, // D
    _unused: u3 = 0, // --
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

    // run every second CPU cycle, flip on every tick
    odd_cycle: bool = false,

    // tracks the CPU cycles since the last output sample. This supports downsampling
    sample_clock: f64 = 0,

    // Samples waiting to be sent to the sound card
    samples: [1024]f32 = undefined,
    // And how many are filled in
    sample_count: usize = 0,

    pub fn writeRegister(self: *APU, addr: u16, val: u8) void {
        switch (addr) {
            0x4000...0x4003 => self.pulse1.write(@truncate(addr), val),
            0x4004...0x4007 => self.pulse2.write(@truncate(addr), val),
            0x4015 => {
                const status: ChannelEnable = @bitCast(val);
                self.pulse1.setCounter(status.pulse1);
                self.pulse2.setCounter(status.pulse2);
            },
            else => {},
        }
    }

    pub fn readRegister(self: *APU, addr: u16) u8 {
        _ = self;
        _ = addr;
        return 0;
    }

    // takeSample drains the buffer and returns the samples recorded since last call.
    pub fn takeSample(self: *APU) []const f32 {
        const out = self.samples[0..self.sample_count];
        self.sample_count = 0;
        return out;
    }

    pub fn tick(self: *APU) void {
        self.odd_cycle = !self.odd_cycle;
        if (self.odd_cycle) {
            self.pulse1.clockTimer();
            self.pulse2.clockTimer();
        }

        // Every ~40.58 CPU cycles, record one sample for the sound card
        self.sample_clock += 1;
        if (self.sample_clock >= cycles_per_sample) { // once ~40.57 cycles have passed, cut sample
            self.sample_clock -= cycles_per_sample;
            if (self.sample_count < self.samples.len) { // guard array
                self.samples[self.sample_count] = self.sample();
                self.sample_count += 1;
            }
        }
    }

    // sample mixes the pulse channels into one value (~0.0 to 1.0) using
    // pulse_lookup_table
    // TODO: add triangle/noise/dmc once those exists
    pub fn sample(self: *const APU) f32 {
        // widen so the two u4s can fit , u4 larges is 15
        const pulse_sum = @as(u5, self.pulse1.output()) + self.pulse2.output();
        return pulse_lookup_table[pulse_sum];
    }
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

test "length counter loads and resets on $4015" {
    var apu: APU = .{};
    // enable
    apu.writeRegister(0x4015, 0b01);
    apu.writeRegister(0x4003, 0b00001_000); // length index 1-> 254
    try std.testing.expect(apu.pulse1.counter_enabled);
    try std.testing.expectEqual(254, apu.pulse1.length_counter);

    // and disable
    apu.writeRegister(0x4015, 0b00);
    try std.testing.expect(!apu.pulse1.counter_enabled);

    // writing to 4003 should be ignored as counter is disabled
    apu.writeRegister(0x4003, 0b00001_000); // length index 1-> 254
    try std.testing.expectEqual(0, apu.pulse1.length_counter);
}

test "length counter counts down and stops at 0" {
    var apu: APU = .{};
    // enable
    apu.writeRegister(0x4015, 0b01);
    apu.writeRegister(0x4003, 0b00011_000); // length index 3 -> 2 ticks
    try std.testing.expectEqual(2, apu.pulse1.length_counter);

    // tick down to 0
    var expected = apu.pulse1.length_counter;
    while (expected > 0) {
        apu.pulse1.clockLength();
        expected -= 1;
        try std.testing.expectEqual(expected, apu.pulse1.length_counter);
    }

    // do one final tick - should be at 0 and not wrap to 255
    apu.pulse1.clockLength();
    try std.testing.expectEqual(0, apu.pulse1.length_counter);

    // also test the halt branch
    apu.writeRegister(0x4015, 0b01);
    apu.writeRegister(0x4000, 0b00_1_0_0000); // L = 1 = halt
    apu.writeRegister(0x4003, 0b00011_000); // length index 3 -> 2 ticks

    apu.pulse1.clockLength(); // tick should not change counter due to halt
    try std.testing.expectEqual(2, apu.pulse1.length_counter);
}

// duty tests
test "duty sequencer returns expected outputs" {
    for (duty_cycle_sequences, 0..) |pattern, duty| {
        var p: Pulse = .{};
        p.control.duty = @intCast(duty);
        p.timer_period = 0; // advanced on every tick

        for (pattern) |bit| {
            try std.testing.expectEqual(bit, duty_cycle_sequences[p.control.duty][p.sequence_step]);
            p.clockTimer();
        }
        // wrapped back to the start
        try std.testing.expectEqual(0, p.sequence_step);
    }
}

test "$4003 write resets the sequencer phase" {
    var apu: APU = .{};

    // advance arbitrarly
    apu.pulse1.clockTimer();
    apu.pulse1.clockTimer();
    try std.testing.expectEqual(2, apu.pulse1.sequence_step);

    // reset
    apu.writeRegister(0x4003, 0);
    try std.testing.expectEqual(0, apu.pulse1.sequence_step);
}

test "tick records one sample on applicable cpu cycles" {
    var apu: APU = .{};
    for (0..40) |_| apu.tick(); // tick towards ~40.58 CPU cycles
    try std.testing.expectEqual(0, apu.sample_count); // 40 < 40.58; almost there

    apu.tick(); // 41 - we're due!
    try std.testing.expectEqual(1, apu.sample_count);
}

test "mixer" {
    var apu: APU = .{};

    // sample is silent when nothing is playing
    try std.testing.expectEqual(0, apu.sample());

    // place a pulse in a state where output() would return a non-zero volume
    testSetPlaying(&apu.pulse1, 5);
    testSetPlaying(&apu.pulse2, 2);

    // 5 is the index and sum of both pulse values (5 + 2)
    try std.testing.expectEqual(pulse_lookup_table[7], apu.sample());
}

// testSetPlaying sets pulse state
fn testSetPlaying(p: *Pulse, volume: u4) void {
    p.length_counter = 10; // arbitrary as long as not zero
    p.timer_period = 8; // any value >= 8 is fine
    p.control.volume = volume;
    p.control.duty = 2; // 50% duty cycle sequence pattern
    p.sequence_step = 1; // duty 1 = sound
}
