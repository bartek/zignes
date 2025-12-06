const std = @import("std");
const c = @import("cpu.zig");
const panic = std.debug.panic;

pub const Op = enum(u8) {
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    BRK,
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
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    Implicit,
    Implied,
    Undefined,
};

// opcode, addressing mode, cycles
pub const Instruction = struct { Op, AddressMode, u8 };

// makeLookupTable creates the lookup table for all possible instructions.
fn makeLookupTable() [256]Instruction {
    comptime { // guarantee table will be evaluated at compile-time.
        var instr_lookup_table: [256]Instruction = .{UndefinedInstruction} ** 256;

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
