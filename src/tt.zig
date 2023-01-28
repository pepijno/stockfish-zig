const std = @import("std");
const assert = std.debug.assert;
const ArrayList = std.ArrayList;

const hashkey = @import("hashkey.zig");
const types = @import("types.zig");
const move = @import("move.zig");
const Move = move.Move;
const Value = types.Value;
const Depth = types.Depth;
const Bound = types.Bound;
const Key = hashkey.Key;

const TTEntry = struct {
    key16: u16,
    depth8: u8,
    gen_bound8: u8,
    move16: u16,
    value16: i16,
    eval16: i16,

    pub fn save(self: *@This(), k: Key, v: Value, pv: bool, b: Bound, d: Depth, m: Move, ev: Value) void {
        if (@bitCast(u16, m) != 0 or @intCast(u16, k) != self.key16) {
            self.move16 = @bitCast(u16, m);
        }

        if (b == .bound_exact or @intCast(u16, k) != self.key16 or @enumToInt(d) - types.depth_offset + 2 * @boolToInt(pv) > self.depth8 - 4) {
            assert(d > types.depth_offset);
            assert(d < 256 + types.depth_offset);

            self.key16 = @intCast(u16, k);
            self.depth8 = @intCast(u8, d - types.depth_offset);
            self.gen_bound8 = @intCast(u8, tt.generation8 | @as(u8, @boolToInt(pv)) << 2 | @intCast(u8, b));
            self.value16 = @intCast(u16, @enumToInt(v));
            self.eval16 = @intCast(u16, @enumToInt(ev));
        }
    }
};

pub var tt: TranspositionTable = .{
    .cluster_count = 0,
    .table = ArrayList(TranspositionTable.Cluster).init(std.heap.page_allocator),
    .generation8 = 0,
};

pub const TranspositionTable = struct {
    const cluster_size: usize = 3;
    const Cluster = struct {
        entry: [cluster_size]TTEntry,
        padding: [2]u8, // pad to 32 bytes
    };

    comptime {
        assert(@sizeOf(Cluster) == 32);
    }

    const generation_bits: u6 = 3;
    const generation_delta = (1 << generation_bits);
    const generation_cycle = 255 + (1 << generation_bits);
    const generation_mask = (0xFF << generation_bits) & 0xFF;

    cluster_count: usize,
    table: ArrayList(Cluster),
    generation8: u8,

    pub fn init(self: *@This(), allocator: std.mem.Allocator) void {
        self.cluster_count = 0;
        self.table = ArrayList(Cluster).init(allocator);
        self.generation8 = 0;
    }

    pub fn newSearch(self: *@This()) void {
        self.generation8 += generation_delta;
    }

    pub fn firstEntry(self: *@This(), key: Key) []TTEntry {
        return &self.table.items[types.mulHi64(key, self.cluster_count)].entry[0..];
    }

    pub fn resize(self: *@This(), mb_size: usize) void {
        self.cluster_count = mb_size * 1024 * 1024 / @sizeOf(Cluster);
        self.clear();
    }

    pub fn clear(self: *@This()) void {
        // TODO multithread
        const empty_entry = std.mem.zeroes(TTEntry);
        self.table.replaceRange(0, self.cluster_count, [_]TTEntry{empty_entry} ** self.cluster_count);
    }

    pub fn probe(self: @This(), key: Key, found: *bool) *TTEntry {
        const tte: []TTEntry = self.firstEntry(key);
        const key16 = @intCast(u16, key);

        var i: usize = 0;
        while (i < cluster_size) : (i += 1) {
            if (tte[i].key16 == key16 or tte[i].depth8 == 0) {
                tte[i].gen_bound8 = @intCast(u8, self.generation8 | (tte[i].gen_bound8 & (generation_delta - 1)));

                found.* = tte[i].depth8 != 0;
                return &tte[i];
            }
        }

        var replace: *TTEntry = &tte[0];
        i = 0;
        while (i < cluster_size) : (i += 1) {
            if (replace.depth8 - ((generation_cycle + self.generation8 - replace.gen_bound8) & generation_mask)
                > tte[i].depth8 - ((generation_cycle + self.generation8 - tte[i].gen_bound8) & generation_mask)) {
                replace = &tte[i];
            }
        }

        found.* = false;
        return replace;
    }

    pub fn hashfull(self: @This()) u32 {
        var cnt: u32 = 0;
        var i: u32 = 0;
        while (i < 1000) : (i += 1) {
            var j: u32 = 0;
            while (j < cluster_size) : (j += 1) {
                cnt += @boolToInt(self.table.items[j].depth8 and (self.table.items[i].entry[j].gen_bound8 & generation_mask) == self.generation8);
            }
        }

        return cnt / cluster_size;
    }
};
