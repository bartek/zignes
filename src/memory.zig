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
        switch (addr) {
            0x0000...0x1FFF => {
                return self.Ram[addr % 0x0800];
            },
            else => {
                return self.Ram[addr % 0x0800];
            },
        }
    }

    pub fn write(self: *Memory, addr: u16, data: u8) void {
        self.Ram[addr % 0x0800] = data;
    }
};
