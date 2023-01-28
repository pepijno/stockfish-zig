const std = @import("std");

const position = @import("position.zig");
const movegen = @import("movegen.zig");
const move = @import("move.zig");
const types = @import("types.zig");
const Depth = types.Depth;

pub fn perft(comptime root: bool, allocator: std.mem.Allocator, pos: *position.Position, depth: Depth) u64 {
    var out = std.io.getStdOut().writer();
    var buffer = std.io.bufferedWriter(out);
    var buf_out = buffer.writer();

    var start = std.time.milliTimestamp();

    var cnt: u64 = 0;
    var nodes: u64 = 0;
    const leaf = (depth == 2);

    var st = std.mem.zeroes(position.StateInfo);
    var ml: movegen.MoveList = .{};
    ml.generate(.legal, pos.*);

    var cur: usize = 0;
    while (cur < ml.current) : (cur += 1) {
        const m: move.Move = ml.moves[cur].move;

        if (root and depth <= 1) {
            cnt = 1;
            nodes += 1;
        } else {
            pos.doMoveWithoutCheck(m, &st);
            if (leaf) {
                var mll: movegen.MoveList = .{};
                mll.generate(.legal, pos.*);
                cnt = mll.current;
            } else {
                cnt = perft(false, allocator, pos, depth - 1);
            }
            nodes += cnt;
            pos.undoMove(m);
        }
        if (root) {
            buf_out.print("{s}: {}\n", .{m.toString(allocator), cnt}) catch unreachable;
            buffer.flush() catch unreachable;
        }
    }

    var end = std.time.milliTimestamp();

    if (root) {
        buf_out.print("Time: {}s\n", .{@intToFloat(f32, end - start) / 1000.0}) catch unreachable;
        buffer.flush() catch unreachable;
    }

    return nodes;
}
