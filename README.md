# zignes

## Native (SDL)

    zig build run -- <path-to-rom>

## Web (WASM)

Build the module:

    zig build wasm

Outputs `zig-out/wasm/zignes.wasm`. Serve the project root over HTTP so the
browser can fetch the wasm and a ROM:

    python3 -m http.server 8000

Then open <http://localhost:8000/web/>.

## Tests

    zig test src/cpu.zig
