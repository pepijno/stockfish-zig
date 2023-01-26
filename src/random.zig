const std = @import("std");

pub const Random = struct {
    s: u64,

    pub fn create(seed: u64) @This() {
        return .{
            .s = seed,
        };
    }

    pub fn rand(self: *@This()) u64 {
        self.s ^= self.s >> 12;
        self.s ^= self.s << 25;
        self.s ^= self.s >> 27;
        return self.s *% 2685821657736338717;
    }

    pub fn sparseRand(self: *@This()) u64 {
        return self.rand() & self.rand() & self.rand();
    }
};
