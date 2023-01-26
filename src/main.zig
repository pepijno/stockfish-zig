const std = @import("std");
const bitboard = @import("bitboard.zig");
const types = @import("types.zig");
const position = @import("position.zig");
const Position = position.Position;
const StateInfo = position.StateInfo;
const move = @import("move.zig");
const movegen = @import("movegen.zig");

const psqt = @import("psqt.zig");

const allocator = std.testing.allocator;

// const startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
const testFEN = "rnbqkbn1/4pppP/8/2pR4/1pPpP3/5N2/3P1PPp/RNBQKBN1 b Qq c3 1 2";

pub fn main() anyerror!void {
    bitboard.init();

    var state_info: StateInfo = .{
        .pawn_key = 0,
        .material_key = 0,
        .non_pawn_material = [_]types.Value{types.Value.value_draw} ** types.n_colors,
        .castling_rights = types.CastlingRights.any_castling,
        .rule_50 = 0,
        .plies_from_null = 0,
        .en_passant = null,
        .key = 0,
        .checkers_bb = 0,
        .previous = null,
        .blockers_for_king = [_]bitboard.Bitboard{0, 0},
        .pinners = [_]bitboard.Bitboard{0, 0},
        .check_squares = [_]bitboard.Bitboard{0} ** types.n_piece_types,
        .captured_piece = types.Piece.no_piece,
        .repitition = 0,
        .dirty_piece = .{
            .dirty_num = 0,
            .piece = [_]types.Piece{types.Piece.no_piece} ** 3,
            .from = [_]types.Square{types.Square.a1} ** 3,
            .to = [_]types.Square{types.Square.a1} ** 3,
        },
    };

    var new_state_info = state_info;

    var b: Position = Position.emptyBoard(&state_info);
    b.state_info = &state_info;
    // b.resetBoard();
    // b.parseFEN(testFEN);
    try b.print(std.heap.page_allocator);
    b.set(testFEN, false, &state_info);
    try b.print(std.heap.page_allocator);
    b.doMoveWithoutCheck(move.Move.normalMove(types.Square.d8, types.Square.a5), &new_state_info);
    try b.print(std.heap.page_allocator);

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
    // try movegen.generateMoves(types.PieceType.queen, types.Color.black, false, b, &ml, b.by_color_bb[@enumToInt(types.Color.White)]);
    // ml.print();
    //
    // bitboard.pretty(b.by_type_bb[@enumToInt(types.PieceType.all_pieces)]);
}

// test "" {
//     _ = @import("header.zig");
//     _ = @import("image-buffer.zig");
//     _ = @import("color.zig");
//     _ = @import("encoder.zig");
//     _ = @import("decoder.zig");
// }
