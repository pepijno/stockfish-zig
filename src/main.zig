const std = @import("std");
const bitboard = @import("bitboard.zig");
const types = @import("types.zig");
const position = @import("position.zig");
const Position = position.Position;
const StateInfo = position.StateInfo;
const move = @import("move.zig");
const movegen = @import("movegen.zig");
const search = @import("search.zig");

// const start_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
// const test_fen = "rnbqk2r/4pppP/8/2pR4/1pPpP3/5N2/3P1PPp/RNBQKBN1 b Qkq c3 1 2";
// const test_fen = "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1";
const test_fen = "8/PPPk4/8/8/8/8/4Kppp/8 b - - 0 1";

pub fn main() anyerror!void {
    bitboard.init();

    var state_info1: StateInfo = std.mem.zeroes(StateInfo);
    // var state_info2: StateInfo = std.mem.zeroes(StateInfo);
    // var state_info3: StateInfo = std.mem.zeroes(StateInfo);
    // var state_info4: StateInfo = std.mem.zeroes(StateInfo);
    // var state_info5: StateInfo = std.mem.zeroes(StateInfo);

    var pos: Position = Position.emptyBoard(&state_info1);
    pos.set(test_fen, false, &state_info1);
    // pos.doMoveWithoutCheck(move.Move.promotionMove(.g2, .f1, .queen), &state_info2);
    // pos.doMoveWithoutCheck(move.Move.normalMove(.a7, .a6), &state_info3);
    // pos.doMoveWithoutCheck(move.Move.normalMove(.f1, .e2), &state_info4);
    // pos.doMoveWithoutCheck(move.Move.normalMove(.a6, .a5), &state_info5);
    try pos.print(std.heap.page_allocator);

    const nodes: u64 = search.perft(true, std.heap.page_allocator, &pos, 6);
    std.log.info("Nodes searched: {}", .{nodes});

    // var ml: movegen.MoveList = .{};
    // ml.generate(.legal, pos);
    // ml.print();
}

// test "" {
//     _ = @import("header.zig");
//     _ = @import("image-buffer.zig");
//     _ = @import("color.zig");
//     _ = @import("encoder.zig");
//     _ = @import("decoder.zig");
// }
