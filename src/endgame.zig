const std = @import("std");
const assert = std.debug.assert;

const types = @import("types.zig");
const Color = types.Color;
const Value = types.Value;
const Square = types.Square;
const Rank = types.Rank;
const File = types.File;
const ScaleFactor = types.ScaleFactor;
const hashkey = @import("hashkey.zig");
const Key = hashkey.Key;
const position = @import("position.zig");
const Position = position.Position;
const StateInfo = position.StateInfo;
const bb = @import("bitboard.zig");

const EndgameCode = enum {
    evaluation_functions,
    knnk,
    knnkp,
    kxk,
    kbnk,
    kpk,
    krkp,
    krkb,
    krkn,
    kqkp,
    kqkr,

    scaling_functions,
    kbpsk,
    kqkrps,
    krpkr,
    krpkb,
    krppkrp,
    kpsk,
    kbpkb,
    kbppkb,
    kbpkn,
    kpkp,
};

inline fn pushToEdge(sq: Square) Value {
    const rd = bb.rankEdgeDistance(sq.rank());
    const fd = bb.fileEdgeDistance(sq.file());
    return @intToEnum(Value, 90 - (7 * fd * fd / 2 + 7 * rd * rd / 2));
}

inline fn pushToCorner(sq: Square) Value {
    const val: i32 = 7 - @as(i32, sq.rank()) - @as(i32, sq.file());
    return @intToEnum(Value, if (val < 0) -val else val);
}

inline fn pushClose(sq1: Square, sq2: Square) Value {
    return @intToEnum(Value, 140 - 20 * bb.distance(Square, sq1, sq2));
}

inline fn pushAway(sq1: Square, sq2: Square) Value {
    return @intToEnum(Value, 120).sub(pushClose(sq1, sq2));
}

fn normalize(pos: Position, strong_side: Color, sq: Square) Square {
    assert(pos.countByColor(.pawn, strong_side) == 1);

    var s = sq;
    if (pos.square(.pawn, strong_side).file() >= @enumToInt(File.e)) {
        s = sq.flip();
    }

    return if (strong_side == .white) sq else sq.flipRank();
}

pub fn EgType(comptime e: EndgameCode) type {
    return if (@enumToInt(e) < @enumToInt(EndgameCode.scaling_functions))
        Value
    else
        ScaleFactor;
}

pub fn EndgameBase(comptime T: type) type {
    return struct {
        strong_side: Color,
        weak_side: Color,
        _eval: *const fn (self: @This(), pos: Position) T,

        pub fn eval(self: @This(), pos: Position) T {
            return self._eval(pos);
        }
    };
}

pub fn Endgame(comptime e: EndgameCode) type {
    return struct {
        const BaseType = EndgameBase(EgType(e));

        pub fn init(color: Color) BaseType {
            return .{
                .strong_side = color,
                .weak_side = color.flip(),
                ._eval = &eval,
            };
        }

        pub fn eval(self: BaseType, pos: Position) EgType(e) {
            switch (e) {
                .kxk => {
                    assert(pos.checkers() == 0);

                    // TODO movelist stuff
                    // if (pos.side_to_move == self.weak_side 
                    const strong_king = pos.square(.king, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);

                    var result = pos.nonPawnMaterialByColor(self.strong_side)
                        .add(pos.countByColor(.pawn, self.strong_side).mul(.pawn_value_eg))
                        .add(pushToEdge(weak_king))
                        .add(pushClose(strong_king, weak_king));

                    if (pos.countByColor(.queen, self.strong_side) != 0
                        or pos.countByColor(.rook, self.strong_side) != 0
                        or (pos.countByColor(.bishop, self.strong_side) != 0 and pos.countByColor(.knight, self.strong_side) != 0)
                        or ((pos.piecesByColorAndType(self.strong_side, .bishop) & ~bb.dark_squares) != 0 and (pos.piecesByColorAndType(self.strong_side, .bishop) & bb.dark_squares) != 0)) {
                        result = @intToEnum(Value, @minimum(@enumToInt(result.add(.value_known_win)), @enumToInt(.value_tb_win_in_max_ply) - 1));
                    }

                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .kbnk => {
                    const strong_king = pos.square(.king, self.strong_side);
                    const strong_bishop = pos.square(.bishop, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);

                    const result = Value.value_tb_win_in_max_ply.add(@intToEnum(Value, 3520))
                        .add(pushClose(strong_king, weak_king))
                        .add(@intToEnum(Value, 420).mul(pushToCorner(if (bb.oppositeColors(strong_bishop, .a1)) weak_king.flipFile() else weak_king)));

                    assert(@enumToInt(result) <= @enumToInt(Value.value_tb_win_in_max_ply));
                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .kpk => {
                    const strong_king = pos.square(.king, self.strong_side);
                    const strong_pawn = pos.square(.pawn, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);

                    const us: Color = if (self.strong_side == pos.side_to_move) .white else .black;
                    _ = us;
                    _ = weak_king;
                    _ = strong_king;

                    // TODO bitbases probe

                    const result = Value.value_known_win.add(.pawn_value_eg).add(@intToEnum(Value, strong_pawn.rank()));
                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .krkp => {
                    const strong_king = pos.square(.king, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);
                    const strong_rook = pos.square(.rook, self.strong_side);
                    const weak_pawn = pos.square(.pawn, self.weak_side);
                    const queening_square = types.makeSquare(@intToEnum(File, weak_pawn.file()), types.relativeRank(self.weak_side, .rank8));

                    var result: Value = Value.value_draw;

                    if ((bb.forwardFilesBB(self.strong_side, strong_king) & bb.squareBB(weak_pawn)) != 0) {
                        result = Value.rook_value_eg.sub(@intToEnum(Value, bb.distance(Square, strong_king, weak_pawn)));
                    } else if (bb.distance(Square, weak_king, weak_pawn) >= 3 + @as(u8, @boolToInt(pos.side_to_move == self.weak_side))
                        and bb.distance(Square, weak_king, strong_rook) >= 3) {
                        result = Value.rook_value_eg.sub(@intToEnum(Value, bb.distance(Square, strong_king, weak_pawn)));
                    } else if (@enumToInt(types.relativeRank(self.strong_side, @intToEnum(Rank, weak_king.rank()))) <= @enumToInt(Rank.rank3)
                        and bb.distance(Square, weak_king, weak_pawn) == 1
                        and @enumToInt(types.relativeRank(self.strong_side, @intToEnum(Rank, strong_king.rank()))) >= @enumToInt(Rank.rank4)
                        and bb.distance(Square, strong_king, weak_pawn) > 2 + @as(u8, @boolToInt(pos.side_to_move == self.strong_side))) {
                        result = @intToEnum(Value, 80).sub(@intToEnum(Value, 8).mul(@intToEnum(Value, bb.distance(Square, strong_king, weak_pawn))));
                    } else {
                        result = @intToEnum(Value, 200).sub(@intToEnum(Value, 8).mul(
                            @intToEnum(Value, bb.distance(Square, strong_king, weak_pawn.addDirection(types.pawnPush(self.weak_side))))
                            .sub(@intToEnum(Value, bb.distance(Square, weak_king, weak_pawn.addDirection(types.pawnPush(self.weak_side)))))
                            .sub(@intToEnum(Value, bb.distance(Square, weak_pawn, queening_square)))));
                    }
                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .krkb => {
                    const result = pushToEdge(pos.square(.king, self.weak_side));
                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .krkn => {
                    const weak_king = pos.square(.king, self.weak_side);
                    const weak_knight = pos.square(.knight, self.weak_side);
                    const result = pushToEdge(weak_king).add(pushAway(weak_king, weak_knight));
                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .kqkp => {
                    const strong_king = pos.square(.king, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);
                    const weak_pawn = pos.square(.pawn, self.weak_side);

                    var result = pushClose(strong_king, weak_king);

                    if (types.relativeRank(self.weak_side, @intToEnum(Rank, weak_pawn.rank())) != .rank7
                        or bb.distance(Square, weak_king, weak_pawn) != 1
                        or ((bb.file_b_bb | bb.file_d_bb | bb.file_e_bb | bb.file_g_bb) & bb.squareBB(weak_pawn)) != 0) {
                        result = result.add(.queen_value_eg).sub(.pawn_value_eg);
                    }

                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .kqkr => {
                    const strong_king = pos.square(.king, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);

                    const result = Value.queen_value_eg.sub(.rook_value_eg)
                        .add(pushToEdge(weak_king))
                        .add(pushClose(strong_king, weak_king));

                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .knnkp => {
                    const weak_king = pos.square(.king, self.weak_side);
                    const weak_pawn = pos.square(.pawn, self.weak_side);

                    const result = Value.pawn_value_eg
                        .add(pushToEdge(weak_king).mul(@intToEnum(Value, 2)))
                        .sub(@intToEnum(Value, @enumToInt(types.relativeRank(self.weak_side, @intToEnum(Rank, weak_pawn.rank()))) * 10));

                    return if (self.strong_side == pos.side_to_move)
                        result
                    else
                        result.mul(@intToEnum(Value, -1));
                },
                .knnk => {
                    return Value.value_draw;
                },
                .kbpsk => {
                    const strong_pawns = pos.piecesByColorAndType(self.strong_side, .pawn);
                    const all_pawns = pos.piecesByType(.pawn);

                    const strong_bishop = pos.square(.bishop, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);
                    const strong_king = pos.square(.king, self.strong_side);

                    if ((strong_pawns & ~bb.file_a_bb) == 0 or (strong_pawns & ~bb.file_h_bb) == 0) {
                        const queening_square = types.relativeSquare(self.strong_side, types.makeSquare(@intToEnum(File, @intToEnum(Square, @ctz(bb.Bitboard, strong_pawns)).file()), .rank8));

                        if (bb.oppositeColors(queening_square, strong_bishop) and bb.distance(queening_square, weak_king) <= 1) {
                            return ScaleFactor.scale_factor_draw;
                        }
                    }

                    if (((all_pawns & ~bb.file_b_bb) == 0 or (all_pawns & ~bb.file_g_bb) == 0)
                        and pos.nonPawnMaterialByColor(self.weak_side) == 0
                        and pos.countByColor(.pawn, self.weak_side) >= 1) {

                        const weak_pawn = bb.frontMostSquare(self.strong_side, pos.piecesByColorAndType(self.weak_side, .pawn));

                        if (types.relativeRankBySquare(self.strong_side, weak_pawn) == .rank7
                            and (strong_pawns & (weak_pawn.addDirection(types.pawnPush(self.weak_side)))) != 0
                            and (bb.oppositeColors(strong_bishop, weak_pawn) or !bb.moreThanOne(strong_pawns))) {

                            const strong_king_dist = bb.distance(weak_pawn, strong_king);
                            const weak_king_dist = bb.distance(weak_pawn, weak_king);

                            if (@enumToInt(types.relativeRankBySquare(self.strong_side, weak_king)) >= @enumToInt(Rank.rank7)
                                and weak_king_dist <= 2
                                and weak_king_dist <= strong_king_dist) {
                                return ScaleFactor.scale_factor_draw;
                            }
                        }
                    }

                    return ScaleFactor.scale_factor_none;
                },
                .kqkrps => {
                    const strong_king = pos.square(.king, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);
                    const weak_rook = pos.square(.rook, self.weak_side);

                    if (@enumToInt(types.relativeRankBySquare(self.weak_side, weak_king)) <= @enumToInt(Rank.rank2)
                        and @enumToInt(types.relativeRankBySquare(self.weak_side, strong_king)) >= @enumToInt(Rank.rank4)
                        and @enumToInt(types.relativeRankBySquare(self.weak_side, weak_rook)) == @enumToInt(Rank.rank3)
                        and ((pos.piecesByColorAndType(self.weak_side, .pawn) & bb.attacksBB(.king, weak_king) & bb.pawnAttackSpan(self.strong_side, weak_rook)) != 0)) {
                        return ScaleFactor.scale_factor_draw;
                    }

                    return ScaleFactor.scale_factor_draw;
                },
                .krpkr => {
                    const strong_king = pos.square(.king, self.strong_side);
                    const strong_rook = pos.square(.rook, self.strong_side);
                    const strong_pawn = pos.square(.pawn, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);
                    const weak_rook = pos.square(.rook, self.weak_side);

                    const pawn_file = strong_pawn.file();
                    const pawn_rank = strong_pawn.rank();
                    const queening_square = types.makeSquare(@intToEnum(File, pawn_file), .rank8);
                    const tempo = pos.side_to_move == self.strong_side;

                    if (pawn_rank <= @enumToInt(Rank.rank5)
                        and bb.distance(Square, weak_king, queening_square) <= 1
                        and @enumToInt(strong_king) <= @enumToInt(Square.h5)
                        and (weak_rook.rank() == @enumToInt(Rank.rank6) or (pawn_file <= @enumToInt(Rank.rank3) and @intToEnum(Rank, strong_rook.rank()) != .rank6))) {
                        return ScaleFactor.scale_factor_draw;
                    }

                    if (@intToEnum(Rank, pawn_rank) == Rank.rank6
                        and bb.distance(Square, weak_king, queening_square) <= 1
                        and strong_king.rank() + @as(u8, @boolToInt(tempo)) <= @enumToInt(Rank.rank6)
                        and (weak_rook.rank() == @enumToInt(Rank.rank1) or (!tempo and bb.distance(File, weak_rook, strong_pawn) >= 3))) {
                        return ScaleFactor.scale_factor_draw;
                    }

                    if (pawn_rank >= @enumToInt(Rank.rank6)
                        and weak_king == queening_square
                        and weak_rook.rank() == @enumToInt(Rank.rank1)
                        and (!tempo or bb.distance(File, strong_king, strong_pawn) >= 3)) {
                        return ScaleFactor.scale_factor_draw;
                    }

                    if (strong_pawn == .a7
                        and strong_rook == .a8
                        and (weak_king == .h7 or weak_king == .g7)
                        and weak_rook.file() == @enumToInt(File.a)
                        and (weak_rook.rank() <= @enumToInt(Rank.rank3) or strong_king.file() >= @enumToInt(File.d) or strong_king.rank() <= @enumToInt(Rank.rank5))) {
                        return ScaleFactor.scale_factor_draw;
                    }

                    if (pawn_rank <= @enumToInt(Rank.rank5)
                        and weak_king == strong_pawn.addDirection(.north)
                        and @as(i16, bb.distance(Square, strong_king, strong_pawn)) - @as(i16, @boolToInt(tempo)) >= 2
                        and @as(i16, bb.distance(Square, strong_king, weak_rook)) - @as(i16, @boolToInt(tempo)) >= 2) {
                        return ScaleFactor.scale_factor_draw;
                    }

                    if (pawn_rank == @enumToInt(Rank.rank7)
                        and pawn_file != @enumToInt(File.a)
                        and strong_rook.file() == pawn_file
                        and strong_rook != queening_square
                        and (bb.distance(Square, strong_king, queening_square) < @as(i16, bb.distance(Square, weak_king, queening_square)) - 2 + @as(i16, @boolToInt(tempo)))
                        and (bb.distance(Square, strong_king, queening_square) < @as(i16, bb.distance(Square, weak_king, strong_rook)) + @as(i16, @boolToInt(tempo)))) {
                        return @intToEnum(ScaleFactor, @enumToInt(ScaleFactor.scale_factor_max) - 2 * bb.distance(Square, strong_king, queening_square));
                    }

                    if (pawn_file != @enumToInt(File.a)
                        and strong_rook.file() == pawn_file
                        and @enumToInt(strong_rook) < @enumToInt(strong_pawn)
                        and (bb.distance(Square, strong_king, queening_square) < @as(i16, bb.distance(Square, weak_king, queening_square)) - 2 + @as(i16, @boolToInt(tempo)))
                        and (bb.distance(Square, strong_king, strong_pawn.addDirection(.north)) < @as(i16, bb.distance(Square, weak_king, strong_pawn.addDirection(.north))) - 2 + @as(i16, @boolToInt(tempo)))
                        and (bb.distance(Square, weak_king, strong_rook) + @as(u8, @boolToInt(tempo)) >= 3
                            or (bb.distance(Square, strong_king, queening_square) < bb.distance(Square, weak_king, strong_rook) + @as(u8, @boolToInt(tempo))
                                and (bb.distance(Square, strong_king, strong_pawn.addDirection(.north)) < bb.distance(Square, weak_king, strong_pawn) + @as(u8, @boolToInt(tempo)))))) {
                        return @intToEnum(ScaleFactor, @enumToInt(ScaleFactor.scale_factor_max) - 8 * bb.distance(Square, strong_pawn, queening_square) - 2 * bb.distance(Square, strong_king, queening_square));
                    }

                    if (pawn_rank <= @enumToInt(Rank.rank4) and @enumToInt(weak_king) > @enumToInt(strong_pawn)) {
                        if (weak_king.file() == strong_pawn.file()) {
                            return @intToEnum(ScaleFactor, 10);
                        }
                        if (bb.distance(File, weak_king, strong_pawn) == 1 and bb.distance(Square, strong_king, weak_king) > 2) {
                            return @intToEnum(ScaleFactor, 24 - 2 * bb.distance(Square, strong_king, weak_king));
                        }
                    }
                    return ScaleFactor.scale_factor_none;
                },
                .krpkb => {
                    if ((pos.piecesByType(.pawn) & (bb.file_a_bb | bb.file_h_bb)) != 0) {
                        const weak_king = pos.square(.king, self.weak_side);
                        const weak_bishop = pos.square(.bishop, self.weak_side);
                        const strong_king = pos.square(.king, self.strong_side);
                        const strong_pawn: Square = pos.square(.pawn, self.strong_side);
                        const pawn_rank = types.relativeRankBySquare(self.strong_side, strong_pawn);
                        const push = types.pawnPush(self.strong_side);

                        if (pawn_rank == .rank5 and !bb.oppositeColors(weak_bishop, strong_pawn)) {
                            const d = bb.distance(Square, strong_pawn.addDirection(push).addDirection(push).addDirection(push), weak_king);

                            if (d <= 2 and !(d == 0 and weak_king == strong_king.addDirection(push).addDirection(push))) {
                                return @intToEnum(ScaleFactor, 24);
                            } else {
                                return @intToEnum(ScaleFactor, 48);
                            }
                        }

                        if (pawn_rank == .rank6
                            and bb.distance(Square, strong_pawn.addDirection(push).addDirection(push), weak_king) <= 1
                            and (bb.pseudoAttacksBB(.bishop, weak_bishop) & bb.squareBB(strong_pawn.addDirection(push))) != 0
                            and bb.distance(File, weak_bishop, strong_pawn) >= 2) {
                            return @intToEnum(ScaleFactor, 8);
                        }
                    }
                    return ScaleFactor.scale_factor_none;
                },
                .krppkrp => {
                    const strong_pawn1 = @intToEnum(Square, @ctz(bb.Bitboard, pos.piecesByColorAndType(self.strong_side, .pawn)));
                    const strong_pawn2 = @intToEnum(Square, @clz(bb.Bitboard, pos.piecesByColorAndType(self.strong_side, .pawn)));
                    const weak_king = pos.square(.king, self.weak_side);

                    if (pos.pawnPassed(self.strong_side, strong_pawn1) or pos.pawnPassed(self.strong_side, strong_pawn2)) {
                        return ScaleFactor.scale_factor_none;
                    }

                    const pawn_rank = @maximum(@enumToInt(types.relativeRankBySquare(self.strong_side, strong_pawn1)), @enumToInt(types.relativeRankBySquare(self.strong_side, strong_pawn2)));

                    if (bb.distance(File, weak_king, strong_pawn1) <= 1
                        and bb.distance(File, weak_king, strong_pawn2) <= 1
                        and @enumToInt(types.relativeRankBySquare(self.strong_side, weak_king)) > pawn_rank) {
                        return @intToEnum(ScaleFactor, 7 * pawn_rank);
                    }

                    return ScaleFactor.scale_factor_none;
                },
                .kpsk => {
                    const weak_king = pos.square(.king, self.weak_side);
                    const strong_pawns = pos.piecesByColorAndType(self.strong_side, .pawn);

                    if ((strong_pawns & ~(bb.file_a_bb | bb.file_h_bb)) == 0
                        and (strong_pawns & ~bb.passedPawnSpan(self.weak_side, weak_king)) == 0) {
                        return ScaleFactor.scale_factor_draw;
                    }
                    return ScaleFactor.scale_factor_none;
                },
                .kbpkb => {
                    const strong_pawn = pos.square(.pawn, self.strong_side);
                    const strong_bishop = pos.square(.bishop, self.strong_side);
                    const weak_bishop = pos.square(.bishop, self.weak_side);
                    const weak_king = pos.square(.king, self.weak_side);

                    if ((bb.forwardFilesBB(self.strong_side, strong_pawn) & bb.squareBB(weak_king)) != 0
                        and (bb.oppositeColors(weak_king, strong_bishop) or @enumToInt(types.relativeRankBySquare(self.strong_side, weak_king)) <= @enumToInt(Rank.rank6))) {
                        return ScaleFactor.scale_factor_draw;
                    }

                    if (bb.oppositeColors(strong_bishop, weak_bishop)) {
                        return ScaleFactor.scale_factor_draw;
                    }
                    return ScaleFactor.scale_factor_none;
                },
                .kbppkb => {
                    const strong_bishop = pos.square(.bishop, self.strong_side);
                    const weak_bishop = pos.square(.bishop, self.weak_side);

                    if (!bb.oppositeColors(strong_bishop, weak_bishop)) {
                        return ScaleFactor.scale_factor_none;
                    }

                    const weak_king = pos.square(.king, self.weak_side);
                    const strong_pawn1 = @intToEnum(Square, @ctz(bb.Bitboard, pos.piecesByColorAndType(self.strong_side, .pawn)));
                    const strong_pawn2 = @intToEnum(Square, @clz(bb.Bitboard, pos.piecesByColorAndType(self.strong_side, .pawn)));
                    var block_sq1: Square = .a1;
                    var block_sq2: Square = .a1;

                    if (@enumToInt(types.relativeRankBySquare(self.strong_side, strong_pawn1)) > @enumToInt(types.relativeRankBySquare(self.strong_side, strong_pawn2))) {
                        block_sq1 = strong_pawn1.addDirection(types.pawnPush(self.strong_side));
                        block_sq2 = types.makeSquare(@intToEnum(File, strong_pawn2.file()), @intToEnum(Rank, strong_pawn1.rank()));
                    } else {
                        block_sq1 = strong_pawn2.addDirection(types.pawnPush(self.strong_side));
                        block_sq2 = types.makeSquare(@intToEnum(File, strong_pawn1.file()), @intToEnum(Rank, strong_pawn2.rank()));
                    }

                    switch (bb.distance(File, strong_pawn1, strong_pawn2)) {
                        0 => {
                            if (weak_king.file() == block_sq1.file()
                                and @enumToInt(types.relativeRankBySquare(self.strong_side, weak_king)) >= @enumToInt(types.relativeRankBySquare(self.strong_side, block_sq1))
                                and bb.oppositeColors(weak_king, strong_bishop)) {
                                return ScaleFactor.scale_factor_draw;
                            } else {
                                return ScaleFactor.scale_factor_none;
                            }
                        },
                        1 => {
                            if (weak_king == block_sq1
                                and bb.oppositeColors(weak_king, strong_bishop)
                                and (weak_bishop == block_sq2
                                    or (bb.attacksBB(.bishop, block_sq2, pos.pieces()) & pos.piecesByColorAndType(self.weak_side, .bishop)) != 0
                                    or bb.distance(Rank, strong_pawn1, strong_pawn2) >= 2)) {
                                return ScaleFactor.scale_factor_draw;
                            } else if (weak_king == block_sq2
                                and bb.oppositeColors(weak_king, strong_bishop)
                                and (weak_bishop == block_sq1
                                    or (bb.attacksBB(.bishop, block_sq1, pos.pieces()) & pos.piecesByColorAndType(self.weak_side, .bishop)) != 0)) {
                                return ScaleFactor.scale_factor_draw;
                            } else {
                                return ScaleFactor.scale_factor_none;
                            }
                        },
                        else => {
                            return ScaleFactor.scale_factor_none;
                        }
                    }
                },
                .kbpkn => {
                    const strong_pawn = pos.square(.pawn, self.strong_side);
                    const strong_bishop = pos.square(.bishop, self.strong_side);
                    const weak_king = pos.square(.king, self.weak_side);

                    if (weak_king.file() == strong_pawn.file()
                        and @enumToInt(types.relativeRankBySquare(self.strong_side, strong_pawn)) < @enumToInt(types.relativeRankBySquare(self.strong_side, weak_king))
                        and (bb.oppositeColors(weak_king, strong_bishop) or @enumToInt(types.relativeRankBySquare(self.strong_side, weak_king)) <= @enumToInt(Rank.rank6))) {
                        return ScaleFactor.scale_factor_draw;
                    }
                    return ScaleFactor.scale_factor_none;
                },
                .kpkp => {
                    const strong_king = normalize(pos, self.strong_side, pos.square(.king, self.strong_side));
                    const weak_king = normalize(pos, self.strong_side, pos.square(.king, self.weak_side));
                    const strong_pawn = normalize(pos, self.strong_side, pos.square(.pawn, self.strong_side));

                    const us = if (self.strong_side == pos.side_to_move) Color.white else Color.black;

                    if (strong_pawn.rank() >= @enumToInt(Rank.rank5) and strong_pawn.file() != @enumToInt(File.a)) {
                        return ScaleFactor.scale_factor_none;
                    }

                    // TODO return bitbase probe
                    return ScaleFactor.scale_factor_none;
                },
                else => {
                    if (EgType(e) == Value) {
                        return Value.value_draw;
                    } else {
                        return ScaleFactor.scale_factor_none;
                    }
                },
            }
        }
    };
}

pub const Endgames = struct {
    value_map: std.AutoHashMap(Key, EndgameBase(Value)),
    scale_factor_map: std.AutoHashMap(Key, EndgameBase(ScaleFactor)),

    pub fn init(allocator: std.mem.Allocator) !@This() {
        var endgames: @This() = .{
            .value_map = std.AutoHashMap(Key, EndgameBase(Value)).init(allocator),
            .scale_factor_map = std.AutoHashMap(Key, EndgameBase(ScaleFactor)).init(allocator),
        };

        try endgames.add(.kpk, "KPK");
        try endgames.add(.knnk, "KNNK");
        try endgames.add(.kbnk, "KBNK");
        try endgames.add(.krkp, "KRKP");
        try endgames.add(.krkb, "KRKB");
        try endgames.add(.krkn, "KRKN");
        try endgames.add(.kqkp, "KQKP");
        try endgames.add(.kqkr, "KQKR");
        try endgames.add(.knnkp, "KNNKP");

        try endgames.add(.krpkr, "KRPKR");
        try endgames.add(.krpkb, "KRPKB");
        try endgames.add(.kbpkb, "KBPKB");
        try endgames.add(.kbpkn, "KBPKN");
        try endgames.add(.kbppkb, "KBPPKB");
        try endgames.add(.krppkrp, "KRPPKRP");

        return endgames;
    }

    pub fn map(self: @This(), comptime T: type) std.AutoHashMap(Key, EndgameBase(T)) {
        return if (T == ScaleFactor)
            self.scale_factor_map
        else
            self.value_map;
    }

    pub fn add(self: *@This(), comptime e: EndgameCode, comptime code: []const u8) !void {
        var state_info = std.mem.zeroes(StateInfo);

        var pos: Position = Position.emptyBoard(&state_info);
        const white_pos = try pos.setForEndgame(code, .white, &state_info);
        const black_pos = try pos.setForEndgame(code, .black, &state_info);
        try self.map(EgType(e)).put(white_pos.materialKey(), Endgame(e).init(.white));
        try self.map(EgType(e)).put(black_pos.materialKey(), Endgame(e).init(.black));
    }

    pub fn probe(self: @This(), comptime T: type, key: Key) ?EndgameBase(T) {
        return self.map(T).get(key);
    }
};
