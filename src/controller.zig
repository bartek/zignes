const std = @import("std");
const Atomic = std.atomic;

pub const Button = enum(u3) { A, B, Select, Start, Up, Down, Left, Right };

// Controller has 8 buttons in fixed order. Games read them by writing 1 then 0 to $4016
// (the "strobe"). Buttons in order:
// A, B, Select, Start, Up, Down, Left, Right
pub const Controller = struct {
    button_state: Atomic.Value(u8) = Atomic.Value(u8).init(0), // bit = 0 = A, bit 1 = B, ... bit 7 = Right
    shift_register: u8 = 0,
    strobe: bool = false,

    pub fn setButton(self: *Controller, button: Button, pressed: bool) void {
        // mask shifts 1 creates a mask with exactly one bit set, the bit for _this_
        // button
        // Button.A      → @intFromEnum = 0 → 1 << 0 = 0b00000001
        // Button.B      → @intFromEnum = 1 → 1 << 1 = 0b00000010
        // Button.Start  → @intFromEnum = 3 → 1 << 3 = 0b00001000
        // Button.Right  → @intFromEnum = 7 → 1 << 7 = 0b10000000
        const mask: u8 = @as(u8, 1) << @intFromEnum(button);

        const current = self.button_state.load(.monotonic);
        // Depending on current button state we do one of:
        // OR-assign. Guarantee bit is set while all others stay the same.
        // button_state = 0b00000100  (Select pressed)
        // mask         = 0b00001000  (Start)
        //        OR  ↓
        // button_state = 0b00001100  (Select AND Start now pressed)
        //
        // Or ..
        //
        // Flip every bit. So where mask had 1, now there's a 0. Clear button bits
        // when button is released.
        // mask         = 0b00001000
        // ~mask        = 0b11110111
        // button_state = 0b00001100
        //        AND ↓
        // button_state = 0b00000100  (Start cleared, Select still pressed)
        const updated = if (pressed) current | mask else current & ~mask;
        self.button_state.store(updated, .monotonic);
    }

    pub fn write(self: *Controller, value: u8) void {
        self.strobe = (value & 1) != 0;
        if (self.strobe) {
            self.shift_register = self.button_state.load(.monotonic);
        }
    }

    pub fn read(self: *Controller) u8 {
        if (self.strobe) {
            // While strobe is high, always return button A's current state
            return self.button_state.load(.monotonic) & 1;
        }

        // Strobe is low, return LSB of shift register, then shift right
        const bit = self.shift_register & 1;
        self.shift_register >>= 1;
        return bit;
    }
};
