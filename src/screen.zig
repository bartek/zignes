const std = @import("std");
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

const PPU = @import("ppu.zig").PPU;

// Screen is the screen handling module using SDL2
pub const Screen = struct {
    window: *c.SDL_Window,
    renderer: *c.SDL_Renderer,
    texture: *c.SDL_Texture,

    // NES screen dimensions
    width: i32 = 256,
    height: i32 = 240,

    pub fn init() !Screen {
        if (c.SDL_Init(c.SDL_INIT_VIDEO) < 0) {
            std.debug.print("SDL initialization failed: {s}\n", .{c.SDL_GetError()});
            return error.SDLInitFailed;
        }

        const window = c.SDL_CreateWindow(
            "zignes",
            c.SDL_WINDOWPOS_CENTERED,
            c.SDL_WINDOWPOS_CENTERED,
            256 * 2,
            240 * 2,
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

        return Screen{
            .window = window,
            .renderer = renderer,
            .texture = texture,
        };
    }

    pub fn deinit(self: *Screen) void {
        c.SDL_DestroyTexture(self.texture);
        c.SDL_DestroyRenderer(self.renderer);
        c.SDL_DestroyWindow(self.window);
        c.SDL_Quit();
    }

    // Render the PPU buffer to the screen
    pub fn render(self: *Screen, ppu: *const PPU) !void {
        _ = ppu;

        // Generate simple gradient pattern
        var pixels: [256 * 240 * 4]u8 = undefined;

        var i: usize = 0;
        while (i < 256 * 240) : (i += 1) {
            const x = i % 256;
            const gray: u8 = @intCast(x);
            const pixel_idx = i * 4;
            pixels[pixel_idx] = gray; // R
            pixels[pixel_idx + 1] = gray; // G
            pixels[pixel_idx + 2] = gray; // B
            pixels[pixel_idx + 3] = 255; // A
        }

        const result = c.SDL_UpdateTexture(self.texture, null, &pixels, 256 * 4);
        if (result != 0) {
            std.debug.print("Failed to update texture: {s}\n", .{c.SDL_GetError()});
            return error.TextureUpdateFailed;
        }

        _ = c.SDL_RenderClear(self.renderer);
        _ = c.SDL_RenderCopy(self.renderer, self.texture, null, null);
        c.SDL_RenderPresent(self.renderer);
    }
};
