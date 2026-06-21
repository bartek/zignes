const std = @import("std");

const APU = @import("apu.zig").APU;
const Bus = @import("bus.zig").Bus;
const CPU = @import("cpu.zig").CPU;
const Cartridge = @import("cartridge.zig").Cartridge;
const Controller = @import("controller.zig").Controller;
const NESBus = @import("bus.zig").NESBus;
const PPU = @import("ppu.zig").PPU;

const Allocator = std.mem.Allocator;

pub const NES = struct {
    cart: Cartridge,
    cpu: CPU,
    ppu: PPU,
    apu: APU = .{},
    controller: Controller = .{},
    bus: Bus,
    ram: [0x800]u8 = .{0} ** 0x800,

    pub fn loadROMFromFile(self: *NES, allocator: Allocator, io: std.Io, file_path: []const u8) !void {
        self.cart = try Cartridge.loadFromFile(allocator, io, file_path);
        self.initAfterCart();
    }

    pub fn load(self: *NES, allocator: Allocator, bytes: []const u8) !void {
        self.cart = try Cartridge.load(allocator, bytes);
        self.initAfterCart();
    }

    fn initAfterCart(self: *NES) void {
        self.ram = .{0} ** 0x800;
        self.apu = .{};
        self.controller = .{};
        self.ppu = .{ .cart = &self.cart };
        self.bus = .{ .nesBus = NESBus.init(&self.ram, &self.ppu, &self.controller, &self.cart) };
        self.cpu = CPU.init(&self.bus);

        // Read reset vector to set CPU entry point.
        // The iNES header (parsed in cartridge.zig) tells us the PRG-ROM and CHR-ROM
        // sizes. PRG-ROM is mapped into CPU address space, so we can read the reset
        // vector at $FFFC-$FFFD to find where execution begins.
        const lo: u16 = self.bus.read(0xFFFC);
        const hi: u16 = self.bus.read(0xFFFD);
        self.cpu.PC = (hi << 8) | lo;
    }

    // tick returns true when NMI just fired (vblank reached)
    pub fn tick(self: *NES) bool {
        _ = self.cpu.tick();

        // TODO: apu tick the same as CPU?
        self.apu.tick();

        // CPU runs at 1/3 speed of PPU, so tick PPU 3x for every CPU tick
        self.ppu.tick();
        self.ppu.tick();
        self.ppu.tick();

        // Deliver NMI from PPU to CPU
        if (self.ppu.nmi_triggered) {
            self.cpu.interrupt = .nmi;
            self.ppu.nmi_triggered = false;
            return true; // frame complete
        }
        return false;
    }

    pub fn deinit(self: *NES) void {
        self.cart.deinit();
    }
};
