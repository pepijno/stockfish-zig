const std = @import("std");
const bitboard = @import("bitboard.zig");
const types = @import("types.zig");
const Board = @import("board.zig").Board;
const move = @import("move.zig");
const movegen = @import("movegen.zig");

const allocator = std.testing.allocator;

// const startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
const testFEN = "rnbqkbn1/pp1ppppP/8/2pR4/1pPpP3/5N2/PP1P1PPp/RNBQKBN1 b KQkq c3 1 2";

pub fn main() anyerror!void {
    bitboard.init();

    var b: Board = Board.emptyBoard();
    b.resetBoard();
    b.parseFEN(testFEN);
    b.print();

    // var ml: movegen.MoveList = .{
    //     .moves = std.ArrayList(movegen.ExtMove).init(allocator),
    // };
    // const m = move.Move.castlingMove(types.Square.a1, types.Square.a8);
    // try ml.moves.append(.{
    //     .move = m,
    //     .value = 0,
    // });
    // ml.print();
    //
    // try movegen.generateMoves(types.PieceType.queen, types.Color.black, false, b, &ml, b.byColorBB[@enumToInt(types.Color.White)]);
    // ml.print();

    bitboard.pretty(b.byTypeBB[@enumToInt(types.PieceType.all_pieces)]);
}

// test "" {
//     _ = @import("header.zig");
//     _ = @import("image-buffer.zig");
//     _ = @import("color.zig");
//     _ = @import("encoder.zig");
//     _ = @import("decoder.zig");
// }
