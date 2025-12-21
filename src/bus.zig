pub const Bus = struct {
    Ram: []u8,

    pub fn init(
        ram: []u8,
    ) Bus {
        return Bus{
            .Ram = ram,
        };
    }

    pub fn read(self: *Bus, addr: u16) u8 {
        return self.Ram[addr];
    }

    pub fn write(self: *Bus, addr: u16, data: u8) void {
        self.Ram[addr] = data;
    }
};
