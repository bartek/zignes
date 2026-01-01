const std = @import("std");
const Bus = @import("bus.zig").Bus;
const CPUTestBus = @import("bus.zig").CPUTestBus;
const PPU = @import("ppu.zig").PPU;
const instructions = @import("instructions.zig");
const Instruction = instructions.Instruction;
const Op = instructions.Op;
const panic = std.debug.panic;

const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

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
    PageSize: u16 = 256, // Each page in RAM is 256 bytes
    X: u8 = 0, // Register
    Y: u8 = 0, // Register
    A: u8 = 0, // Accumulator
    P: Flags = 0, // Status flags
    SP: u8 = 0, // Stack Pointer
    PC: u16 = 0, // Program counter

    Cycles: u64 = 0, // Number of cycles executed
    Halt: u8 = 0,

    interrupt: Interrupt = .none,

    allocator: Allocator,
    Bus: *Bus,

    pub fn init(allocator: Allocator, bus: *Bus) CPU {
        return CPU{
            .allocator = allocator,
            .Bus = bus,
        };
    }

    // tick ticks the CPU and returns true if halted
    pub fn tick(self: *CPU) bool {
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

        const opcode = self.fetchOpcode(self.Bus);
        const instruction = instructions.decodeInstruction(opcode);

        // Decrement cycle counter, handling the case where instruction[2] is already 0
        if (instruction[2] > 0) {
            self.Halt +%= instruction[2] - 1;
        }
        return self.Halt > 0;
    }

    fn runFromState(self: *CPU, initial_state: *const CPUState) !CPUState {
        self.PC = initial_state.pc;
        self.SP = initial_state.s;
        self.A = initial_state.a;
        self.X = initial_state.x;
        self.Y = initial_state.y;
        self.P = @bitCast(initial_state.p);

        for (initial_state.ram) |entry| {
            self.Bus.write(entry[0], entry[1]);
        }

        // exec a single instruction
        const instruction = instructions.decodeInstruction(self.fetchOpcode(self.Bus));

        self.exec(instruction);

        var final_ram = try self.allocator.alloc(struct { u16, u8 }, initial_state.ram.len);
        for (0..initial_state.ram.len) |i| {
            const entry = &initial_state.ram[i];
            const addr = entry[0];
            assert(i < final_ram.len);
            final_ram[i] = .{ entry[0], self.Bus.read(addr) };
        }

        return .{ .pc = self.PC, .a = self.A, .x = self.X, .s = self.SP, .y = self.Y, .p = @bitCast(self.P), .ram = final_ram };
    }

    fn fetchOpcode(self: *CPU, mem: *Bus) u8 {
        const opcode = mem.read(self.PC);
        self.PC +%= 1;
        return opcode;
    }

    // conditionalBranch performs a conditional branch based on the provided condition.
    fn conditionalBranch(self: *CPU, condition: bool) void {
        if (condition) {
            self.Halt += 1; // +1 if branch taken
            // PC = PC + 2 + memory (signed)

            // The offset is signed, meaning it may be a jump forward or backward.
            const offset: i8 = @bitCast(self.nextOp());

            const current_pc: i32 = self.PC;
            const new_pc: u32 = @bitCast(current_pc + offset);
            self.PC = @truncate(new_pc);

            // And add another cycle if branch crosses page boundary
            // High byte of the address (& 0xff00) identifies the page.
            if (current_pc & 0xff00 != new_pc & 0xff00) {
                self.Halt += 1;
            }
        } else {
            self.PC +%= 1;
        }
    }

    fn exec(self: *CPU, instruction: *const Instruction) void {
        const op = instruction[0];
        switch (op) {
            Op.ADC => self.adc(self.operator(instruction)),
            Op.SBC => self.adc(~self.operator(instruction)),
            Op.INC => {
                const addr = self.addressOfInstruction(instruction);
                var val: u8 = self.Bus.read(addr);
                val +%= 1;
                self.Bus.write(addr, val);
                self.setZN(val);
            },
            Op.DEC => {
                const addr = self.addressOfInstruction(instruction);
                var val: u8 = self.Bus.read(addr);
                val -%= 1;
                self.Bus.write(addr, val);
                self.setZN(val);
            },
            Op.INX => {
                self.X +%= 1;
                self.setZN(self.X);
            },
            Op.DEX => {
                self.X -%= 1;
                self.setZN(self.X);
            },
            Op.INY => {
                self.Y +%= 1;
                self.setZN(self.Y);
            },
            Op.DEY => {
                self.Y -%= 1;
                self.setZN(self.Y);
            },
            Op.BCC => self.conditionalBranch(!self.getCarry()),
            Op.BCS => self.conditionalBranch(self.getCarry()),
            Op.BEQ => self.conditionalBranch(self.getZero()),
            Op.BMI => self.conditionalBranch(self.getNegative()),
            Op.BNE => self.conditionalBranch(!self.getZero()),
            Op.BPL => self.conditionalBranch(!self.getNegative()),
            Op.BVC => self.conditionalBranch(!self.getOverflow()),
            Op.BVS => self.conditionalBranch(self.getOverflow()),

            Op.TAX => {
                self.X = self.A;
                self.setZN(self.X);
            },
            Op.TAY => {
                self.Y = self.A;
                self.setZN(self.Y);
            },
            Op.TSX => {
                self.X = self.SP;
                self.setZN(self.X);
            },
            Op.TXA => {
                self.A = self.X;
                self.setZN(self.A);
            },
            Op.TXS => {
                self.SP = self.X;
            },
            Op.TYA => {
                self.A = self.Y;
                self.setZN(self.A);
            },
            Op.LDA => {
                const b = self.operator(instruction);
                self.A = b;
                self.setZN(b);
            },
            Op.LDX => {
                const b = self.operator(instruction);
                self.X = b;
                self.setZN(b);
            },
            Op.PLP => {
                self.P = (self.pop() & 0xCF) | (self.P & 0x30);
            },
            Op.PHP => {
                // Store byte to stack containing status flags with break and bit 5 set (order is NV11DIZC)
                self.push(self.P | flagBreak | (1 << 5));
            },
            Op.PLA => {
                self.A = self.pop();
                self.setZN(self.A);
            },
            Op.PHA => {
                const addr = (0x100 +% @as(u16, self.SP));
                self.Bus.write(addr, self.A);
                self.SP = self.SP -% 1;
            },
            Op.LDY => {
                const b = self.operator(instruction);
                self.Y = b;
                self.setZN(b);
            },
            Op.STA => {
                self.Bus.write(self.addressOfInstruction(instruction), self.A);
            },
            Op.STX => {
                self.Bus.write(self.addressOfInstruction(instruction), self.X);
            },
            Op.STY => {
                self.Bus.write(self.addressOfInstruction(instruction), self.Y);
            },
            Op.RTS => {
                const low: u16 = self.pop();
                const high: u16 = self.pop();
                self.PC = (high << 8) | low;
                self.PC +%= 1;
            },
            Op.JMP => {
                const mode = instruction[1];
                switch (mode) {
                    .Absolute => {
                        self.PC = self.getAddress16();
                    },
                    .Indirect => {
                        const addr = self.getAddress16();
                        const low: u16 = self.Bus.read(addr);
                        var hi_addr: u16 = addr + 1;
                        // 6502 bug: if the low byte is $FF, the high byte wraps
                        // around to the beginning of the same page.
                        // Example: JMP ($30FF) will read the low byte from $30FF
                        // and the high byte from $3000.
                        // This is emulated here.
                        // Ref: https://www.nesdev.org/wiki/6502_addressing_modes#IndirectX
                        if ((addr & 0x00FF) == 0x00FF) {
                            hi_addr = addr & 0xFF00;
                        }
                        const high: u16 = self.Bus.read(hi_addr);
                        self.PC = low | (high << 8);
                    },
                    else => {
                        panic("\n!! not implemented JMP mode 0x{x}\n", .{mode});
                    },
                }
            },
            Op.JSR => {
                const return_addr = self.PC + 2;
                self.push(@intCast(return_addr >> 8));
                self.push(@intCast(return_addr & 0xFF));
                self.PC = self.getAddress16();
            },
            Op.RTI => {
                // pull status flags (ignore bits 4 (break) and 5 (unused). These don't
                // exist as physical registers and are only artifacts in transient
                // scenarios. Thus, they are discarded on pull)
                self.P = (self.pop() & 0xCF) | (self.P & 0x30);

                // pull PC low byte, then high byte
                const lo: u16 = self.pop();
                const hi: u16 = self.pop();
                self.PC = (hi << 8) | lo;
            },
            Op.BRK => {
                self.PC +%= 1;

                // push PC + 1 high byte, then low byte
                self.push(@intCast(self.PC >> 8));
                self.push(@intCast(self.PC & 0xFF));

                // push status flags with break and bit 5 set (NV11DIZC)
                self.push(self.P | flagBreak | (1 << 5));

                // set interrupt disable flag
                self.P |= flagInterrupt;

                // jump to IRQ vector at $FFFE
                const lo: u16 = self.Bus.read(0xFFFE);
                const hi: u16 = self.Bus.read(0xFFFF);
                self.PC = (hi << 8) | lo;
            },
            Op.Undefined => {
                panic("\n!! not implemented 0x{x}\n", .{instruction[2]});
            },
        }
    }

    // adc adds carry flag and memory (arg) value to accumulator. Then sets flags.
    fn adc(self: *CPU, arg: u8) void {
        const v: u16 = @as(u16, self.A) + @as(u16, arg) + @intFromBool(self.getCarry());
        self.setZN(v);
        self.setCarry(v);
        self.setOverflow(((self.A ^ v) & (arg ^ v) & 0x80) != 0);
        self.A = @truncate(v);
    }

    fn operator(self: *CPU, instruction: *const Instruction) u8 {
        const addr = self.addressOfInstruction(instruction);
        return self.Bus.read(addr);
    }

    fn addressOfInstruction(self: *CPU, instruction: *const Instruction) u16 {
        const mode = instruction[1];
        switch (mode) {
            .Immediate => {
                // Uses the 8-bit operand itself as the value for the operation, rather
                // than fetching a value from a memory address.
                const addr = self.PC;
                self.PC +%= 1;
                return addr;
            },
            .ZeroPage => {
                // Fetches the value from an 8-bit address on the zero page.
                const b = self.nextOp();
                return b;
            },
            .ZeroPageX => {
                var b: u16 = self.nextOp();
                b += self.X;
                b = b % self.PageSize;
                return b;
            },
            .ZeroPageY => {
                var b: u16 = self.nextOp();
                b += self.Y;
                b = b % self.PageSize;
                return b;
            },
            .Absolute => {
                return self.getAddress16();
            },
            .AbsoluteX => {
                const addr = self.getAddress16();
                const result, _ = @addWithOverflow(addr, self.X);
                return result;
            },
            // Relative used only by branch instructions, code should not reach here as
            // never obtaining address of instruction
            .Relative => unreachable,
            .AbsoluteY => {
                const addr = self.getAddress16();
                const result, _ = @addWithOverflow(addr, self.Y);
                return result;
            },
            .Indirect => unreachable, // not handled here (yet?)
            .IndirectX => {
                // Jump to the address found at the formula
                // val = PEEK(PEEK((arg + X) % 256) + PEEK((arg + X + 1) % 256) * 256)
                // Basically .. get the address (from zero page memory) at (arg + X),
                // then get the next address (wrapping +1 add) from memory
                // Then bitwise OR to combine into a full address.
                const addrLow = self.nextOp() +% self.X;
                const low: u16 = self.Bus.read(addrLow);
                const addrHi = addrLow +% 1;
                const high: u16 = self.Bus.read(addrHi);
                return low | (high << 8);
            },
            .IndirectY => {
                // Different Jump from IndirectX:
                // val = PEEK(PEEK(arg) + PEEK((arg + 1) % 256) * 256 + Y)
                // Get the address from zero page memory at arg,
                // then get the next address (add + 1w/ overflow) from memory
                // Then bitwise OR to combine into a full address, then add Y.
                // Basically same as IndirectX but add Y at the end instead of X at the beginning.
                const addrLow: u8 = self.nextOp();
                const low: u16 = self.Bus.read(addrLow);
                const addrHi, _ = @addWithOverflow(addrLow, 1);
                const high: u16 = self.Bus.read(addrHi);
                const base_addr = low | (high << 8);
                return base_addr +% @as(u16, self.Y);
            },
            .Implicit => unreachable,
            .Implied => unreachable,
            .Undefined => unreachable,
        }
    }

    // getAddress16 reads the next two bytes from the program counter and bitwise OR
    // combines them into their proper positions within a 16-bit address.
    fn getAddress16(self: *CPU) u16 {
        const low: u16 = self.nextOp();
        const high: u16 = self.nextOp();
        return low | (high << 8);
    }

    fn nextOp(self: *CPU) u8 {
        const b = self.Bus.read(self.PC);
        self.PC +%= 1;
        return b;
    }

    // push a byte onto the stack and then decrement the stack pointer.
    fn push(self: *CPU, b: u8) void {
        self.Bus.write(0x100 + @as(u16, self.SP), b);
        self.SP -%= 1;
    }

    // pop a byte from the stack and increment the stack pointer.
    // note the stack lives in page 0x100-0x1FF. The CPI has the 8-bit stack pointer
    // and the address is formed like so: $0100 + SP
    fn pop(self: *CPU) u8 {
        self.SP +%= 1;
        return self.Bus.read(0x100 + @as(u16, self.SP));
    }

    fn getNegative(self: *CPU) bool {
        return (self.P & flagNegative) != 0;
    }

    fn getZero(self: *CPU) bool {
        return (self.P & flagZero) != 0;
    }

    fn getOverflow(self: *CPU) bool {
        return (self.P & flagOverflow) != 0;
    }

    fn getCarry(self: *CPU) bool {
        return (self.P & flagCarry) != 0;
    }

    fn setFlag(self: *CPU, flag: Flags, value: bool) void {
        if (value) {
            self.P |= flag;
        } else {
            self.P &= ~flag;
        }
    }

    // setZero accepts a full 16-bit result and sets zero based on the low byte.
    fn setZero(self: *CPU, value: u16) void {
        self.setFlag(flagZero, (value & 0xff) == 0);
    }

    fn setNegative(self: *CPU, value: u16) void {
        self.setFlag(flagNegative, value & 0x80 != 0);
    }

    fn setZN(self: *CPU, value: u16) void {
        self.setZero(value);
        self.setNegative(value);
    }

    fn setCarry(self: *CPU, value: u16) void {
        self.setFlag(flagCarry, value > 0xFF);
    }

    fn setOverflow(self: *CPU, value: bool) void {
        self.setFlag(flagOverflow, value);
    }
};

const T = std.testing;

fn parseCPUTestCase(allocator: Allocator, testcase_str: []const u8) !std.json.Parsed([]InstrTest) {
    return try std.json.parseFromSlice(
        []InstrTest,
        allocator,
        testcase_str,
        .{ .ignore_unknown_fields = true },
    );
}

// runTestCase runs a test case by provisioning a CPU, setting initial state, and
// checking received state against expected.
fn runTestCase(test_case: *const InstrTest) !void {
    const allocator = std.heap.page_allocator;
    const ram = try allocator.alloc(u8, 0x10000);
    const testBus = CPUTestBus{ .mem = ram };
    var bus = Bus{ .cpuTestBus = testBus };
    var cpu = CPU.init(T.allocator, &bus);

    const received = try cpu.runFromState(&test_case.initial);
    defer T.allocator.free(received.ram);
    const expected = &test_case.final;
    try T.expectEqual(expected.pc, received.pc);
    try T.expectEqual(expected.s, received.s);
    try T.expectEqual(expected.a, received.a);
    try T.expectEqual(expected.x, received.x);
    try T.expectEqual(expected.y, received.y);
    try T.expectEqual(expected.p, received.p);
}

// runTestsForInstruction runs all tests for a given `hex` instruction
fn runTestsForInstruction(hex: []const u8) !void {
    const instr_file = try std.mem.concat(
        T.allocator,
        u8,
        &[_][]const u8{ hex, ".json" },
    );

    defer T.allocator.free(instr_file);

    const file_path = try std.fs.path.join(
        T.allocator,
        &[_][]const u8{
            "src",
            "tests",
            "nes6502",
            "v1",
            instr_file,
        },
    );
    defer T.allocator.free(file_path);

    const contents = try std.fs.cwd().readFileAlloc(T.allocator, file_path, std.math.maxInt(usize));
    defer T.allocator.free(contents);

    var parsed = try parseCPUTestCase(T.allocator, contents);
    defer parsed.deinit();

    for (0..parsed.value.len) |i| {
        if (runTestCase(&parsed.value[i])) |_| {} else |err| {
            std.debug.print("Failed to run test case {d} for instruction {s}\n", .{ i, hex });
            return err;
        }
    }
}

const InstrTest = struct {
    name: []const u8,
    initial: CPUState,
    final: CPUState,
};

// State of CPU used for point-in-time tests
pub const CPUState = struct {
    const Self = @This();
    const Cell = struct { u16, u8 };
    pc: u16,
    s: u8,
    a: u8,
    x: u8,
    y: u8,
    p: Flags,
    ram: []Cell,
};

// ref: https://www.nesdev.org/wiki/Instruction_reference
test "TAX, TAY, TSX, TXA, TXS, TYA" {
    try runTestsForInstruction("aa");
    try runTestsForInstruction("a8");
    try runTestsForInstruction("ba");
    try runTestsForInstruction("8a");
    try runTestsForInstruction("9a");
    try runTestsForInstruction("98");
}

test "LDA, LDX, LDY" {
    try runTestsForInstruction("a9");
    try runTestsForInstruction("a5");
    try runTestsForInstruction("b5");
    try runTestsForInstruction("ad");
    try runTestsForInstruction("bd");
    try runTestsForInstruction("b9");
    try runTestsForInstruction("a1");
    try runTestsForInstruction("b1");

    try runTestsForInstruction("a2");
    try runTestsForInstruction("a6");
    try runTestsForInstruction("b6");
    try runTestsForInstruction("ae");
    try runTestsForInstruction("be");

    try runTestsForInstruction("a0");
    try runTestsForInstruction("a4");
    try runTestsForInstruction("b4");
    try runTestsForInstruction("ac");
    try runTestsForInstruction("bc");
}

test "STA, STX, STY" {
    try runTestsForInstruction("85");
    try runTestsForInstruction("95");
    try runTestsForInstruction("8d");
    try runTestsForInstruction("9d");
    try runTestsForInstruction("99");
    try runTestsForInstruction("81");
    try runTestsForInstruction("91");

    try runTestsForInstruction("86");
    try runTestsForInstruction("96");
    try runTestsForInstruction("8e");

    try runTestsForInstruction("84");
    try runTestsForInstruction("94");
    try runTestsForInstruction("8c");
}

test "BCC, BCS, BEQ, BMI, BNE, BPL, BVC, BVS" {
    try runTestsForInstruction("90");
    try runTestsForInstruction("b0");
    try runTestsForInstruction("f0");
    try runTestsForInstruction("30");
    try runTestsForInstruction("d0");
    try runTestsForInstruction("10");
    try runTestsForInstruction("50");
    try runTestsForInstruction("70");
}

test "JMP, JSR, RTS, BRK, RTI" {
    try runTestsForInstruction("4c");
    try runTestsForInstruction("6c");
    try runTestsForInstruction("20");
    try runTestsForInstruction("60");
    try runTestsForInstruction("00");
    try runTestsForInstruction("40");
}

test "PHA, PHP, PLA, PLP" {
    try runTestsForInstruction("48");
    try runTestsForInstruction("08");
    try runTestsForInstruction("68");
    try runTestsForInstruction("28");
}

test "ADC, SBC, INC, DEC, INX, DEX, INY, DEY" {
    try runTestsForInstruction("69");
    try runTestsForInstruction("65");
    try runTestsForInstruction("75");
    try runTestsForInstruction("6d");
    try runTestsForInstruction("7d");
    try runTestsForInstruction("79");
    try runTestsForInstruction("61");
    try runTestsForInstruction("71");

    try runTestsForInstruction("e9");
    try runTestsForInstruction("e5");
    try runTestsForInstruction("f5");
    try runTestsForInstruction("ed");
    try runTestsForInstruction("fd");
    try runTestsForInstruction("f9");
    try runTestsForInstruction("e1");
    try runTestsForInstruction("f1");

    try runTestsForInstruction("e6");
    try runTestsForInstruction("f6");
    try runTestsForInstruction("ee");
    try runTestsForInstruction("fe");
    try runTestsForInstruction("c6");
    try runTestsForInstruction("d6");
    try runTestsForInstruction("ce");
    try runTestsForInstruction("de");

    try runTestsForInstruction("e8");
    try runTestsForInstruction("ca");
    try runTestsForInstruction("c8");
    try runTestsForInstruction("88");
}
