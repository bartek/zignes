const std = @import("std");
const Memory = @import("memory.zig").Memory;

pub const Flags = u8;

pub const flagCarry: Flags = 1 << 0;
pub const flagZero: Flags = 1 << 1;
pub const flagInterrupt: Flags = 1 << 2;
pub const flagDecimal: Flags = 1 << 3;
pub const flagBreak: Flags = 1 << 4;
pub const flagOverflow: Flags = 1 << 6;
pub const flagNegative: Flags = 1 << 7;

pub const Interrupt = enum(u8) {
    none = 0,
    nmi = 1,
    irq = 2,
};

pub const CPU = struct {
    X: u8 = 0, // Register
    Y: u8 = 0, // Register
    A: u8 = 0, // Accumulator
    P: Flags = 0, // Status flags
    SP: u8 = 0, // Stack Pointer
    PC: u16 = 0, // Program counter

    Cycles: u64 = 0, // Number of cycles executed
    Halt: i8 = 0,

    interrupt: Interrupt = .none,

    pub fn init() CPU {
        return CPU{};
    }

    pub fn tick(self: *CPU, mem: *Memory) bool {
        self.Cycles += 1;

        if (self.Halt > 0) {
            self.Halt -= 1;
            return self.Halt == 0;
        }

        // TODO: interrupts
        switch (self.interrupt) {
            .irq => {}, // call irq func
            .nmi => {}, // call nmi func
            // then for both above, self.interrupt = .none
            else => {},
        }

        const opcode = self.fetchOpcode(mem);
        std.debug.print("Opcode: {x}\n", .{opcode});
        //const instr = &Opcodes[opcode];
        return false;
    }

    fn fetchOpcode(self: *CPU, mem: *Memory) u8 {
        const opcode = mem.read(self.PC);
        self.PC +%= 1;
        return opcode;
    }
};

pub const Opcode = struct {
    size: u8,
    cycles: i32,
    //addrMode: AddrMode,
    //handler: fn (cpu: *CPU, mem: *Memory, operand: Operand) void,
};

pub var Opcodes: [256]Opcode = undefined;
