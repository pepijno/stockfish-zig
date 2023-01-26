const std = @import("std");
const assert = std.debug.assert;

const types = @import("types.zig");
const Position = @import("position.zig").Position;
const bb = @import("bitboard.zig");
const move = @import("move.zig");
const Bitboard = bb.Bitboard;
const Color = types.Color;
const Square = types.Square;
const PieceType = types.PieceType;
const Direction = types.Direction;
const Move = move.Move;

pub const ExtMove = struct {
    move: move.Move,
    value: i32,
};

pub const MAX_MOVES: usize = 256;

pub const MoveGenType = enum {
    captures,
    quiets,
    quiet_checks,
    evasions,
    non_evasions,
    legal,
};

pub fn MoveList(comptime move_gen_type: MoveGenType) type {
    return struct {
        moves: [MAX_MOVES]ExtMove = undefined,
        current: usize = 0,

        pub fn print(self: @This()) void {
            _ = move_gen_type;
            std.debug.print("Size: {}\n", .{self.moves.items.len});
            var i = 0;
            while (i < self.current) : (i += 1) {
                const extMove = self.moves[i];
                std.debug.print("{{ move: {}, value: {} }}\n", .{ extMove.move, extMove.value });
            }
        }

        pub fn addMove(self: *@This(), m: move.Move) void {
            self.moves[self.current] = m;
            self.current += 1;
        }
    };
}

pub fn generate(comptime move_gen_type: MoveGenType, position: Position, move_list: *MoveList) void {
    if (move_gen_type == MoveGenType.legal) {
        // TODO
        // const us = position.colorToMove();
        // const pinned = position.blockersForKing(us) & position.piecesByColor(us);
        // const king_square = position.square(PieceType.king, us);
        //
        // var cur: *MoveList = move_list;
        //
        //
        //
    } else {
        assert((move_gen_type == MoveGenType.evasions) == (position.checkers() != 0));

        const us = position.sideToMove();

        if (us == Color.white) {
            generateAll(move_gen_type, Color.white, position, move_list);
        } else {
            generateAll(move_gen_type, Color.black, position, move_list);
        }
    }
}

fn makePromotions(comptime gen_type: MoveGenType, comptime direction: types.Direction, move_list: *MoveList, to: Square) void {
    _ = direction;
    _ = move_list;
    _ = to;
    if (gen_type == MoveGenType.captures or gen_type == MoveGenType.evasions or gen_type == MoveGenType.non_evasions) {
        move_list.addMove(Move.promotionMove(to.subDirection(direction), to, PieceType.queen));
    }

    if (gen_type == MoveGenType.quiets or gen_type == MoveGenType.evasions or gen_type == MoveGenType.non_evasions) {
        move_list.addMove(Move.promotionMove(to.subDirection(direction), to, PieceType.rook));
        move_list.addMove(Move.promotionMove(to.subDirection(direction), to, PieceType.bishop));
        move_list.addMove(Move.promotionMove(to.subDirection(direction), to, PieceType.knight));
    }
}

fn generatePawnMoves(comptime move_gen_type: MoveGenType, comptime us: Color, position: Position, move_list: *MoveList, target: Bitboard) void {
    // zig fmt: off
    const them: Color           = comptime us.flip();
    const t_rank_7_bb: Bitboard = comptime if (us == Color.white) bb.rank_7_bb else bb.rank_2_bb;
    const t_rank_2_bb: Bitboard = comptime if (us == Color.white) bb.rank_3_bb else bb.rank_6_bb;
    const up: Direction         = comptime types.pawnPush(us);
    const up_right: Direction   = comptime if (us == Color.white) Direction.north_east else Direction.south_west;
    const up_left: Direction    = comptime if (us == Color.white) Direction.north_west else Direction.south_east;

    const empty_squares: Bitboard = ~position.pieces();
    const enemies: Bitboard       = if (move_gen_type == MoveGenType.evasions) position.checkers() else position.piecesByColor(them);

    const pawns_on_7: Bitboard     = position.pieces(us, PieceType.pawn) &  t_rank_7_bb;
    const pawns_not_on_7: Bitboard = position.pieces(us, PieceType.pawn) & ~t_rank_7_bb;
    // zig fmt: on

    move_list.moves.clearAndFree();

    if (move_gen_type != MoveGenType.captures) {
        var b1: Bitboard = bb.shift(pawns_not_on_7, up) & empty_squares;
        var b2: Bitboard = bb.shift((b1 & t_rank_2_bb), up) & empty_squares;

        if (move_gen_type == MoveGenType.evasions) {
            b1 &= target;
            b2 &= target;
        }

        if (move_gen_type == MoveGenType.quiet_checks) {
            const king_square = position.square(PieceType.king, them);

            // TODO
            const dc_candidate_pawns: Bitboard = position.blockersForKing(king_square) & ~bb.squareFileBB(king_square);
            b1 &= bb.pawnAttacksBySquare(them, king_square) | bb.shift(up, dc_candidate_pawns);
            b2 &= bb.pawnAttacksBySquare(them, king_square) | bb.shift(up, bb.shift(up, dc_candidate_pawns));
        }

        while (b1 != 0) {
            const sq: Square = bb.popLsb(b1);
            move_list.addMove(Move.normalMove(sq.subDirection(up), sq));
        }

        while (b2 != 0) {
            const sq: Square = bb.popLsb(b2);
            move_list.addMove(Move.normalMove(sq.subDirection(up).subDirection(up), sq));
        }
    }

    if (pawns_on_7 != 0) {
        var b1: Bitboard = bb.shift(pawns_on_7, up_right) & enemies;
        var b2: Bitboard = bb.shift(pawns_on_7, up_left) & enemies;
        var b3: Bitboard = bb.shift(pawns_on_7, up) & empty_squares;

        while (b1 != 0) {
            makePromotions(move_gen_type, up_right, bb.popLsb(b1), move_list);
        }

        while (b2 != 0) {
            makePromotions(move_gen_type, up_left, bb.popLsb(b2), move_list);
        }

        while (b3 != 0) {
            makePromotions(move_gen_type, up, bb.popLsb(b3), move_list);
        }
    }

    if (move_gen_type == MoveGenType.captures or move_gen_type == MoveGenType.evasions or move_gen_type == MoveGenType.non_evasions) {
        var b1 = bb.shift(pawns_not_on_7, up_right) & enemies;
        var b2 = bb.shift(pawns_not_on_7, up_left) & enemies;

        while (b1 != 0) {
            const sq = bb.popLsb(b1);
            move_list.addMove(Move.normalMove(sq.subDirection(up_right), sq));
        }

        while (b2 != 0) {
            const sq = bb.popLsb(b2);
            move_list.addMove(Move.normalMove(sq.subDirection(up_left), sq));
        }

        if (position.en_passant) |ep| {
            assert(if (us == Color.white) ep.rank() == 5 else ep.rank() == 2);

            if (move_gen_type == MoveGenType.evasions and (target & (bb.squareBB(ep.addDirection(up)))) != 0) {
                return;
            }

            b1 = pawns_not_on_7 & bb.pawnAttacksBySquare(them, ep);

            while (b1 != 0) {
                const sq = bb.popLsb(b1);
                move_list.addMove(Move.enPassantMove(sq, ep));
            }
        }
    }
}

fn generateMoves(comptime piece_type: PieceType, comptime side: Color, comptime checks: bool, position: Position, move_list: *MoveList, target: Bitboard) void {
    comptime {
        assert(piece_type != PieceType.king and piece_type != PieceType.pawn);
    }

    var bitboard: Bitboard = position.pieces(side, piece_type);

    while (bitboard != 0) {
        const from = bb.popLsb(bitboard);

        const b: Bitboard = bb.attacksBB(piece_type, from, position.allPieces()) & target;

        if (checks and (piece_type == PieceType.queen or (position.blockersForKing(side.flip()) & bb.squareBB(from)) == 0)) {
            b &= position.checkSquare(piece_type);
        }

        while (b != 0) {
            move_list.addMove(Move.normalMove(from, bb.popLsb(b)));
        }
    }
}

fn generateAll(comptime move_gen_type: MoveGenType, comptime us: Color, position: Position, move_list: *MoveList) void {
    comptime {
        assert(move_gen_type != MoveGenType.legal);
    }

    const checks = comptime move_gen_type == MoveGenType.quiet_checks;
    const king_square = position.square(PieceType.king, us);
    var target: Bitboard = 0;

    if (move_gen_type != MoveGenType.evasions or !bb.moreThanOne(position.checkers())) {
        target = switch (move_gen_type) {
            MoveGenType.evasions => bb.betweenBB(king_square, @ctz(Bitboard, position.checkers())),
            MoveGenType.non_evasions => ~position.piecesByColor(us),
            MoveGenType.captures => position.piecesByColor(us.flip()),
            else => ~position.allPieces(),
        };

        generatePawnMoves(move_gen_type, us, position, move_list, target);
        generateMoves(PieceType.knight, us, checks, position, move_list, target);
        generateMoves(PieceType.bishop, us, checks, position, move_list, target);
        generateMoves(PieceType.roo, us, checks, position, move_list, target);
        generateMoves(PieceType.queen, us, checks, position, move_list, target);
    }

    if (!checks or position.blockersForKing(us.flip()) & bb.squareBB(king_square) != 0) {
        var b = bb.attacksBB(PieceType.king, king_square) & (if (move_gen_type == MoveGenType.evasions) ~position.piecesByColor(us) else target);
        if (checks) {
            b &= ~bb.attacksBB(PieceType.queen, position.square(PieceType.king, us.flip()));
        }

        while (b != 0) {
            move_list.addMove(Move.normalMove(king_square, bb.popLsb(b)));
        }

        if ((move_gen_type == MoveGenType.quiets or move_gen_type == MoveGenType.non_evasions) and position.colorCanCastle(us)) {
            const rights = if (us == Color.white) [_]types.CastlingRights{ types.CastlingRights.white_queen_side_castle, types.CastlingRights.white_king_side_castle }
                        else [_]types.CastlingRights{ types.CastlingRights.black_queen_side_castle, types.CastlingRights.black_king_side_castle };
            for (rights) |cr| {
                if (!position.castlingImpeded(cr) and position.canCastle(cr)) {
                    move_list.addMove(Move.castlingMove(king_square, position.castlingRookSquare(cr)));
                }
            }
        }
    }
}
