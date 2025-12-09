const std = @import("std");
const c = @import("cpu.zig");
const panic = std.debug.panic;

pub const Op = enum(u8) {
    BCC,
    BCS,
    BEQ,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    LDA,
    LDX,
    LDY,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    Undefined,
};

// ref: https://www.masswerk.at/6502/6502_instruction_set.html
//
// Addressing Mode:
//
// Mode               | Meaning / Example
// -------------------+--------------------------------------------------------------
// Immediate          | Value is inside the instruction.
//                    | Example: LDA #$0A  (load A with the literal value $0A)
//
// Zero Page          | Instruction gives an 8-bit address in the first 256 bytes.
//                    | Example: LDA $42
//
// Zero Page,X (or Y) | Zero-page address plus the X (or Y) register.
//                    | Example: LDA $42,X
//
// Absolute           | Instruction gives a full 16-bit memory address.
//                    | Example: LDA $1234
//
// Absolute,X (or Y)  | Absolute address plus X (or Y).
//                    | Example: LDA $1234,X
//
// Indirect (various) | Instruction gives a pointer; CPU uses that pointer (plus
//                    | optional register) to compute the final address.
//                    | Example: LDA ($40),Y
//
// Accumulator / Implied
//                    | No operand needed; instruction acts directly on A or is
//                    | self-contained.
//                    | Example: ASL A
//
pub const AddressMode = enum {
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Immediate,
    Implicit,
    Implied,
    IndirectX,
    IndirectY,
    Relative,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Undefined,
};

// opcode, addressing mode, cycles
pub const Instruction = struct { Op, AddressMode, u8 };

// makeLookupTable creates the lookup table for all possible instructions.
fn makeLookupTable() [256]Instruction {
    comptime { // guarantee table will be evaluated at compile-time.
        var instr_lookup_table: [256]Instruction = .{UndefinedInstruction} ** 256;

        // Load A
        instr_lookup_table[0xa9] = .{ Op.LDA, AddressMode.Immediate, 2 };
        instr_lookup_table[0xa5] = .{ Op.LDA, AddressMode.ZeroPage, 3 };
        instr_lookup_table[0xb5] = .{ Op.LDA, AddressMode.ZeroPageX, 4 };
        instr_lookup_table[0xad] = .{ Op.LDA, AddressMode.Absolute, 4 };
        instr_lookup_table[0xbd] = .{ Op.LDA, AddressMode.AbsoluteX, 4 };
        instr_lookup_table[0xb9] = .{ Op.LDA, AddressMode.AbsoluteY, 4 };
        instr_lookup_table[0xa1] = .{ Op.LDA, AddressMode.IndirectX, 6 };
        instr_lookup_table[0xb1] = .{ Op.LDA, AddressMode.IndirectY, 5 };

        // Load Y
        instr_lookup_table[0xa0] = .{ Op.LDY, AddressMode.Immediate, 2 };
        instr_lookup_table[0xa4] = .{ Op.LDY, AddressMode.ZeroPage, 3 };
        instr_lookup_table[0xb4] = .{ Op.LDY, AddressMode.ZeroPageX, 4 };
        instr_lookup_table[0xac] = .{ Op.LDY, AddressMode.Absolute, 4 };
        instr_lookup_table[0xbc] = .{ Op.LDY, AddressMode.AbsoluteX, 4 };

        // Load X
        instr_lookup_table[0xa2] = .{ Op.LDX, AddressMode.Immediate, 2 };
        instr_lookup_table[0xa6] = .{ Op.LDX, AddressMode.ZeroPage, 3 };
        instr_lookup_table[0xb6] = .{ Op.LDX, AddressMode.ZeroPageY, 4 };
        instr_lookup_table[0xae] = .{ Op.LDX, AddressMode.Absolute, 4 };
        instr_lookup_table[0xbe] = .{ Op.LDX, AddressMode.AbsoluteY, 3 };

        // Branch
        instr_lookup_table[0x90] = .{ Op.BCC, AddressMode.Relative, 2 };
        instr_lookup_table[0xb0] = .{ Op.BCS, AddressMode.Relative, 2 };
        instr_lookup_table[0xf0] = .{ Op.BEQ, AddressMode.Relative, 2 };
        instr_lookup_table[0x30] = .{ Op.BMI, AddressMode.Relative, 2 };
        instr_lookup_table[0xd0] = .{ Op.BNE, AddressMode.Relative, 2 };
        instr_lookup_table[0x10] = .{ Op.BPL, AddressMode.Relative, 2 };
        instr_lookup_table[0x50] = .{ Op.BVC, AddressMode.Relative, 2 };
        instr_lookup_table[0x70] = .{ Op.BVS, AddressMode.Relative, 2 };

        // Store A
        instr_lookup_table[0x85] = .{ Op.STA, AddressMode.ZeroPage, 3 };
        instr_lookup_table[0x95] = .{ Op.STA, AddressMode.ZeroPageX, 4 };
        instr_lookup_table[0x8d] = .{ Op.STA, AddressMode.Absolute, 4 };
        instr_lookup_table[0x9d] = .{ Op.STA, AddressMode.AbsoluteX, 5 };
        instr_lookup_table[0x99] = .{ Op.STA, AddressMode.AbsoluteY, 5 };
        instr_lookup_table[0x81] = .{ Op.STA, AddressMode.IndirectX, 6 };
        instr_lookup_table[0x91] = .{ Op.STA, AddressMode.IndirectY, 6 };

        // Store X
        instr_lookup_table[0x86] = .{ Op.STX, AddressMode.ZeroPage, 3 };
        instr_lookup_table[0x96] = .{ Op.STX, AddressMode.ZeroPageX, 4 };
        instr_lookup_table[0x8e] = .{ Op.STX, AddressMode.Absolute, 4 };

        // Store Y
        instr_lookup_table[0x84] = .{ Op.STY, AddressMode.ZeroPage, 3 };
        instr_lookup_table[0x94] = .{ Op.STY, AddressMode.ZeroPageX, 4 };
        instr_lookup_table[0x8c] = .{ Op.STY, AddressMode.Absolute, 4 };

        // Transfer
        instr_lookup_table[0x00] = .{ Op.BRK, AddressMode.Implied, 7 };
        instr_lookup_table[0xaa] = .{ Op.TAX, AddressMode.Implied, 2 };
        instr_lookup_table[0xa8] = .{ Op.TAY, AddressMode.Implied, 2 };
        instr_lookup_table[0xba] = .{ Op.TSX, AddressMode.Implied, 2 };
        instr_lookup_table[0x8a] = .{ Op.TXA, AddressMode.Implied, 2 };
        instr_lookup_table[0x9a] = .{ Op.TXS, AddressMode.Implied, 2 };
        instr_lookup_table[0x98] = .{ Op.TYA, AddressMode.Implied, 2 };
        return instr_lookup_table;
    }
}

pub const UndefinedInstruction: Instruction = .{ Op.Undefined, AddressMode.Undefined, 0 };
const lookup_table = makeLookupTable();

pub fn decodeInstruction(opcode: u8) *const Instruction {
    return &lookup_table[opcode];
}

comptime {
    std.debug.assert(lookup_table.len == 256);
    std.debug.assert(lookup_table[0xa8][0] == Op.TAY);
}
