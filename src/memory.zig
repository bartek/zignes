// Memory is the main memory bus
pub const Memory = struct {
    Ram: []u8,

    pub fn init(
        ram: []u8,
    ) Memory {
        return Memory{
            .Ram = ram,
        };
    }

    pub fn read(self: *Memory, addr: u16) u8 {
        return self.Ram[addr];
    }

    pub fn write(self: *Memory, addr: u16, data: u8) void {
        self.Ram[addr] = data;
    }
};
