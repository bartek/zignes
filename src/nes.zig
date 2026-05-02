const std = @import("std");

const Cartridge = @import("cartridge.zig").Cartridge;
const NESBus = @import("bus.zig").NESBus;
const CPU = @import("cpu.zig").CPU;
const PPU = @import("ppu.zig").PPU;
const Bus = @import("bus.zig").Bus;

const Allocator = std.mem.Allocator;

pub const NES = struct {
    allocator: Allocator,
    cart: *Cartridge,
    cpu: *CPU,
    ppu: *PPU,
    bus: *Bus,

    pub fn loadROMFromFile(allocator: Allocator, file_path: [*:0]const u8) !NES {
        const cart = try allocator.create(Cartridge);
        cart.* = try Cartridge.loadFromFile(allocator, file_path);

        const cpu = try allocator.create(CPU);
        const ppu = try allocator.create(PPU);

        const ram = try allocator.alloc(u8, 0x800);
        const nesBus = NESBus.init(
            ram,
            ppu,
            cart,
        );

        const bus = try allocator.create(Bus);
        bus.* = Bus{ .nesBus = nesBus };

        cpu.* = CPU.init(allocator, bus);
        ppu.* = PPU{ .cart = cart };

        // Read reset vector to set CPU entry point.
        // The iNES header (parsed in cartridge.zig) tells us the PRG-ROM and CHR-ROM
        // sizes. PRG-ROM is mapped into CPU address space, so we can read the reset
        // vector at $FFFC-$FFFD to find where execution begins.
        const lo: u16 = bus.read(0xFFFC);
        const hi: u16 = bus.read(0xFFFD);
        cpu.PC = (hi << 8) | lo;

        return .{
            .allocator = allocator,
            .cart = cart,
            .cpu = cpu,
            .ppu = ppu,
            .bus = bus,
        };
    }

    pub fn tick(self: *NES) void {
        _ = self.cpu.tick();

        // CPU runs at 1/3 speed of PPU, so tick PPU 3x for every CPU tick
        self.ppu.tick();
        self.ppu.tick();
        self.ppu.tick();

        // Deliver NMI from PPU to CPU
        if (self.ppu.nmi_triggered) {
            self.cpu.interrupt = .nmi;
            self.ppu.nmi_triggered = false;
        }
    }

    pub fn deinit(self: *NES) void {
        self.cart.deinit();
        self.allocator.destroy(self.cart);
        self.allocator.destroy(self.cpu);
        self.allocator.destroy(self.ppu);
        self.allocator.destroy(self.bus);
    }
};
