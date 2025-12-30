const std = @import("std");
const c = @cImport({
    @cInclude("SDL2/SDL.h");
    @cInclude("SDL2/SDL_ttf.h");
});

const NES = @import("nes.zig").NES;
const PPU = @import("ppu.zig").PPU;
const Bus = @import("bus.zig").Bus;
const CPU = @import("cpu.zig").CPU;

// Screen is the screen handling module using SDL2
pub const Screen = struct {
    window: *c.SDL_Window,
    renderer: *c.SDL_Renderer,
    texture: *c.SDL_Texture,
    font: *c.TTF_Font,

    // NES screen dimensions
    width: i32 = 256,
    height: i32 = 240,
    // Window dimensions (game + debug side by side)
    window_width: i32 = 1024,
    window_height: i32 = 480,

    pub fn init() !Screen {
        if (c.SDL_Init(c.SDL_INIT_VIDEO) < 0) {
            std.debug.print("SDL initialization failed: {s}\n", .{c.SDL_GetError()});
            return error.SDLInitFailed;
        }

        if (c.TTF_Init() < 0) {
            std.debug.print("SDL_ttf initialization failed: {s}\n", .{c.SDL_GetError()});
            return error.TTFInitFailed;
        }

        const window = c.SDL_CreateWindow(
            "zignes",
            c.SDL_WINDOWPOS_CENTERED,
            c.SDL_WINDOWPOS_CENTERED,
            1024,
            480,
            c.SDL_WINDOW_SHOWN,
        ) orelse {
            std.debug.print("Failed to create window: {s}\n", .{c.SDL_GetError()});
            return error.WindowCreationFailed;
        };

        const renderer = c.SDL_CreateRenderer(window, -1, c.SDL_RENDERER_ACCELERATED) orelse {
            std.debug.print("Failed to create renderer: {s}\n", .{c.SDL_GetError()});
            c.SDL_DestroyWindow(window);
            return error.RendererCreationFailed;
        };

        const texture = c.SDL_CreateTexture(
            renderer,
            c.SDL_PIXELFORMAT_RGB888,
            c.SDL_TEXTUREACCESS_STREAMING,
            256,
            240,
        ) orelse {
            std.debug.print("Failed to create texture: {s}\n", .{c.SDL_GetError()});
            c.SDL_DestroyRenderer(renderer);
            c.SDL_DestroyWindow(window);
            return error.TextureCreationFailed;
        };

        const font = c.TTF_OpenFont("/System/Library/Fonts/Menlo.ttc", 10) orelse {
            std.debug.print("Failed to load font: {s}\n", .{c.SDL_GetError()});
            c.SDL_DestroyTexture(texture);
            c.SDL_DestroyRenderer(renderer);
            c.SDL_DestroyWindow(window);
            return error.FontLoadFailed;
        };

        return Screen{
            .window = window,
            .renderer = renderer,
            .texture = texture,
            .font = font,
        };
    }

    pub fn deinit(self: *Screen) void {
        c.TTF_CloseFont(self.font);
        c.SDL_DestroyTexture(self.texture);
        c.SDL_DestroyRenderer(self.renderer);
        c.SDL_DestroyWindow(self.window);
        c.TTF_Quit();
        c.SDL_Quit();
    }

    // Render the PPU buffer to the screen
    pub fn render(self: *Screen, nes: *NES) !void {
        // Clear renderer with black background
        _ = c.SDL_RenderClear(self.renderer);

        // Render PPU game area (left side)
        try self.renderGameArea(nes.ppu);

        // Render debug info (right side)
        try self.renderDebugArea(nes.bus, nes.cpu);

        c.SDL_RenderPresent(self.renderer);
    }

    fn renderGameArea(self: *Screen, ppu: *const PPU) !void {
        _ = ppu;

        // Draw placeholder for game area (cyan background)
        var rect: c.SDL_Rect = .{
            .x = 0,
            .y = 0,
            .w = 512,
            .h = 480,
        };
        _ = c.SDL_SetRenderDrawColor(self.renderer, 0, 100, 100, 255);
        _ = c.SDL_RenderFillRect(self.renderer, &rect);

        // Draw placeholder text
        try self.renderTextLine("PPU OUTPUT", 200);
    }

    fn renderDebugArea(self: *Screen, bus: *Bus, cpu: *const CPU) !void {
        const allocator = std.heap.page_allocator;

        // Render memory dump as text
        var buf = try std.ArrayList(u8).initCapacity(allocator, 4096);
        defer buf.deinit(allocator);

        const writer = buf.writer(allocator);

        // CPU State Header
        try writer.print("=== CPU STATE ===\n", .{});
        try writer.print("PC: {X:0>4}  A: {X:0>2}  X: {X:0>2}  Y: {X:0>2}\n", .{ cpu.PC, cpu.A, cpu.X, cpu.Y });
        try writer.print("SP: {X:0>2}  P: {X:0>2}  Cycles: {}\n", .{ cpu.SP, cpu.P, cpu.Cycles });
        try writer.print("\n", .{});

        // Flags
        try writer.print("Flags: ", .{});
        if ((cpu.P & 0x80) != 0) try writer.print("N", .{});
        if ((cpu.P & 0x40) != 0) try writer.print("V", .{});
        try writer.print("-", .{});
        if ((cpu.P & 0x10) != 0) try writer.print("B", .{});
        if ((cpu.P & 0x08) != 0) try writer.print("D", .{});
        if ((cpu.P & 0x04) != 0) try writer.print("I", .{});
        if ((cpu.P & 0x02) != 0) try writer.print("Z", .{});
        if ((cpu.P & 0x01) != 0) try writer.print("C", .{});
        try writer.print("\n", .{});

        try writer.print("\n=== MEMORY DUMP ===\n", .{});

        // Render hex dump of first 2KB of RAM
        const mem_size: u16 = 0x800;
        var addr: u16 = 0;

        while (addr < mem_size) : (addr += 16) {
            // Address
            try writer.print("{X:0>4}:  ", .{addr});

            // Hex bytes
            var i: u16 = 0;
            while (i < 16 and addr + i < mem_size) : (i += 1) {
                const byte = bus.read(addr + i);
                try writer.print("{X:0>2} ", .{byte});
            }

            // Padding for partial lines
            if (i < 16) {
                var j = i;
                while (j < 16) : (j += 1) {
                    try writer.print("   ", .{});
                }
            }

            try writer.print("  |", .{});

            // ASCII representation
            i = 0;
            while (i < 16 and addr + i < mem_size) : (i += 1) {
                const byte = bus.read(addr + i);
                if (byte >= 32 and byte < 127) {
                    try writer.print("{c}", .{byte});
                } else {
                    try writer.print(".", .{});
                }
            }

            try writer.print("|\n", .{});
        }

        // Render text line by line on right side (starting at x=512)
        var line_y: i32 = 10;
        var line_start: usize = 0;

        while (line_start < buf.items.len) {
            // Find end of line
            var line_end = line_start;
            while (line_end < buf.items.len and buf.items[line_end] != '\n') : (line_end += 1) {}

            // Render this line
            const line_text = buf.items[line_start..line_end];
            if (line_text.len > 0) {
                try self.renderTextLineAtX(line_text, 522, line_y);
            }

            line_y += 12;
            line_start = line_end + 1;
        }
    }

    fn renderTextLine(self: *Screen, text: []const u8, y: i32) !void {
        try self.renderTextLineAtX(text, 10, y);
    }

    fn renderTextLineAtX(self: *Screen, text: []const u8, x: i32, y: i32) !void {
        // Create C string (null-terminated)
        const allocator = std.heap.page_allocator;
        const c_text = try allocator.allocSentinel(u8, text.len, 0);
        defer allocator.free(c_text);
        @memcpy(c_text, text);

        // Render text to surface
        const white = c.SDL_Color{ .r = 255, .g = 255, .b = 255, .a = 255 };
        const surface = c.TTF_RenderText_Solid(self.font, c_text.ptr, white) orelse {
            std.debug.print("Failed to render text: {s}\n", .{c.SDL_GetError()});
            return error.TextRenderFailed;
        };
        defer c.SDL_FreeSurface(surface);

        // Convert surface to texture
        const texture = c.SDL_CreateTextureFromSurface(self.renderer, surface) orelse {
            std.debug.print("Failed to create texture from surface: {s}\n", .{c.SDL_GetError()});
            return error.TextureCreationFailed;
        };
        defer c.SDL_DestroyTexture(texture);

        // Copy texture to renderer
        var rect: c.SDL_Rect = .{
            .x = x,
            .y = y,
            .w = surface.*.w,
            .h = surface.*.h,
        };
        _ = c.SDL_RenderCopy(self.renderer, texture, null, &rect);
    }
};
