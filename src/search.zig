const std = @import("std");

const position = @import("position.zig");
const movegen = @import("movegen.zig");
const move = @import("move.zig");

pub const Depth = i32;

pub fn perft(comptime root: bool, allocator: std.mem.Allocator, pos: *position.Position, depth: Depth) u64 {
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
            std.log.info("{s}: {}", .{m.toString(allocator), cnt});
        }
    }

    return nodes;
}
