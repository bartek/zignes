// NTSC timing constants shared by emulator core
//
// CPU cycles per second
// https://www.nesdev.org/wiki/Cycle_reference_chart#Clock_rates
pub const cpu_hz: f64 = 1_789_773.0;

// CPU cycles per video frame (PPU's 89342 dots / 3)
// https://www.nesdev.org/wiki/APU_Frame_Counter mentions
// "29780.5 CPU cycles per NTSC PPU frame"
pub const cpu_cycles_per_frame: f64 = 29_780.5;
