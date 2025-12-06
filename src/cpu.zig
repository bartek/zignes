const std = @import("std");
const Memory = @import("memory.zig").Memory;
const instructions = @import("instructions.zig");

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
    X: u8 = 0, // Register
    Y: u8 = 0, // Register
    A: u8 = 0, // Accumulator
    P: Flags = 0, // Status flags
    SP: u8 = 0, // Stack Pointer
    PC: u16 = 0, // Program counter

    Cycles: u64 = 0, // Number of cycles executed
    Halt: i8 = 0,

    interrupt: Interrupt = .none,

    allocator: Allocator,
    Memory: *Memory,

    pub fn init(allocator: Allocator, mem: *Memory) CPU {
        return CPU{
            .allocator = allocator,
            .Memory = mem,
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

        const opcode = self.fetchOpcode(self.Memory);
        const instruction = instructions.operation(self, opcode);

        // TODO: Should this be -1 or is that an implementation detail from reference?
        // Tests should confirm.
        self.Halt += instruction.cycles - 1;

        return false;
    }

    fn runFromState(self: *CPU, initial_state: *const CPUState) !CPUState {
        self.PC = initial_state.pc;
        self.SP = initial_state.s;
        self.A = initial_state.a;
        self.X = initial_state.x;
        self.Y = initial_state.y;
        self.P = @bitCast(initial_state.p);

        for (initial_state.ram) |entry| {
            self.Memory.write(entry[0], entry[1]);
        }

        var final_ram = try self.allocator.alloc(struct { u16, u8 }, initial_state.ram.len);
        for (0..initial_state.ram.len) |i| {
            const entry = &initial_state.ram[i];
            const addr = entry[0];
            assert(i < final_ram.len);
            final_ram[i] = .{ entry[0], self.Memory.read(addr) };
        }

        // exec a single instruction
        // todo: handle  cycles/interrupts?
        _ = instructions.operation(self, self.fetchOpcode(self.Memory));

        return .{ .pc = self.PC, .a = self.A, .x = self.X, .s = self.SP, .y = self.Y, .p = @bitCast(self.P), .ram = final_ram };
    }

    fn fetchOpcode(self: *CPU, mem: *Memory) u8 {
        const opcode = mem.read(self.PC);
        self.PC +%= 1;
        return opcode;
    }

    fn setFlag(self: *CPU, flag: Flags, value: bool) void {
        if (value) {
            self.P |= flag;
        } else {
            self.P &= ~flag;
        }
    }

    pub fn setZero(self: *CPU, value: u8) void {
        self.setFlag(flagZero, value == 0);
    }

    pub fn setNegative(self: *CPU, value: u8) void {
        self.setFlag(flagNegative, value & 0x80 != 0);
    }

    pub fn setZN(self: *CPU, value: u8) void {
        self.setZero(value);
        self.setNegative(value);
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
    const ram = try allocator.alloc(u8, 0x800);
    var memory = Memory{ .Ram = ram };
    var cpu = CPU.init(T.allocator, &memory);

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
test "TAX, TAY" {
    try runTestsForInstruction("aa");
    try runTestsForInstruction("a8");
}
