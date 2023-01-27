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

pub const MoveList = struct {
    moves: [MAX_MOVES]ExtMove = undefined,
    current: usize = 0,

    pub fn print(self: @This()) void {
        std.debug.print("Size: {}\n", .{self.current});
        var i: usize = 0;
        while (i < self.current) : (i += 1) {
            const extMove = self.moves[i];
            std.debug.print("({}): {{ move: {}, value: {} }}\n", .{ i, extMove.move, extMove.value });
        }
    }

    pub fn addMove(self: *@This(), m: move.Move) void {
        self.moves[self.current] = .{
            .move = m,
            .value = 0,
        };
        self.current += 1;
    }

    pub fn byIndex(self: @This(), index: usize) ExtMove {
        return self.moves[index];
    }

    pub fn generate(self: *@This(), comptime move_gen_type: MoveGenType, position: Position) void {
        if (move_gen_type == .legal) {
            const us = position.side_to_move;
            const pinned = position.blockersForKing(us) & position.piecesByColor(us);
            const king_square = position.square(.king, us);

            var cur = self.current;

            if (position.checkers() != 0) {
                self.generate(.evasions, position);
            } else {
                self.generate(.non_evasions, position);
            }

            while (cur != self.current) {
                const m = self.byIndex(cur);
                if (((pinned != 0 and (pinned & bb.squareBB(m.move.from)) != 0) or m.move.from == king_square or m.move.move_type == .en_passant) and !position.legal(m.move)) {
                    std.mem.swap(ExtMove, &self.moves[cur], &self.moves[self.current - 1]);
                    self.current -= 1;
                } else {
                    cur += 1;
                }
            }

        } else {
            assert((move_gen_type == .evasions) == (position.checkers() != 0));

            const us = position.sideToMove();

            if (us == Color.white) {
                self.generateAll(move_gen_type, .white, position);
            } else {
                self.generateAll(move_gen_type, .black, position);
            }
        }
    }

    fn makePromotions(self: *@This(), comptime gen_type: MoveGenType, comptime direction: types.Direction, to: Square) void {
        if (gen_type == .captures or gen_type == .evasions or gen_type == .non_evasions) {
            self.addMove(Move.promotionMove(to.subDirection(direction), to, .queen));
        }

        if (gen_type == .quiets or gen_type == .evasions or gen_type == .non_evasions) {
            self.addMove(Move.promotionMove(to.subDirection(direction), to, .rook));
            self.addMove(Move.promotionMove(to.subDirection(direction), to, .bishop));
            self.addMove(Move.promotionMove(to.subDirection(direction), to, .knight));
        }
    }

    fn generatePawnMoves(self: *@This(), comptime move_gen_type: MoveGenType, comptime us: Color, position: Position, target: Bitboard) void {
        // zig fmt: off
        const them: Color              = comptime us.flip();
        const t_rank_7_bb: Bitboard    = comptime if (us == .white) bb.rank_7_bb else bb.rank_2_bb;
        const t_rank_2_bb: Bitboard    = comptime if (us == .white) bb.rank_3_bb else bb.rank_6_bb;
        const up: Direction            = comptime types.pawnPush(us);
        const up_right: Direction      = comptime if (us == .white) .north_east else .south_west;
        const up_left: Direction       = comptime if (us == .white) .north_west else .south_east;

        const empty_squares: Bitboard  = ~position.pieces();
        const enemies: Bitboard        = if (move_gen_type == .evasions) position.checkers() else position.piecesByColor(them);

        const pawns_on_7: Bitboard     = position.piecesByColorAndType(us, .pawn) &  t_rank_7_bb;
        const pawns_not_on_7: Bitboard = position.piecesByColorAndType(us, .pawn) & ~t_rank_7_bb;
        // zig fmt: on

        self.current = 0;

        if (move_gen_type != .captures) {
            var b1: Bitboard = bb.shift(up, pawns_not_on_7) & empty_squares;
            var b2: Bitboard = bb.shift(up, (b1 & t_rank_2_bb)) & empty_squares;

            if (move_gen_type == .evasions) {
                b1 &= target;
                b2 &= target;
            }

            if (move_gen_type == .quiet_checks) {
                const king_square = position.square(.king, them);
                const dc_candidate_pawns: Bitboard = position.blockersForKing(king_square) & ~bb.squareFileBB(king_square);
                b1 &= bb.pawnAttacksBySquare(them, king_square) | bb.shift(up, dc_candidate_pawns);
                b2 &= bb.pawnAttacksBySquare(them, king_square) | bb.shift(up, bb.shift(up, dc_candidate_pawns));
            }

            while (b1 != 0) {
                const sq: Square = bb.popLsb(&b1);
                self.addMove(Move.normalMove(sq.subDirection(up), sq));
            }

            while (b2 != 0) {
                const sq: Square = bb.popLsb(&b2);
                self.addMove(Move.normalMove(sq.subDirection(up).subDirection(up), sq));
            }
        }

        if (pawns_on_7 != 0) {
            var b1: Bitboard = bb.shift(up_right, pawns_on_7) & enemies;
            var b2: Bitboard = bb.shift(up_left, pawns_on_7) & enemies;
            var b3: Bitboard = bb.shift(up, pawns_on_7) & empty_squares;

            if (move_gen_type == .evasions) {
                b3 &= target;
            }

            while (b1 != 0) {
                self.makePromotions(move_gen_type, up_right, bb.popLsb(&b1));
            }

            while (b2 != 0) {
                self.makePromotions(move_gen_type, up_left, bb.popLsb(&b2));
            }

            while (b3 != 0) {
                self.makePromotions(move_gen_type, up, bb.popLsb(&b3));
            }
        }

        if (move_gen_type == .captures or move_gen_type == .evasions or move_gen_type == .non_evasions) {
            var b1 = bb.shift(up_right, pawns_not_on_7) & enemies;
            var b2 = bb.shift(up_left, pawns_not_on_7) & enemies;

            while (b1 != 0) {
                const sq = bb.popLsb(&b1);
                self.addMove(Move.normalMove(sq.subDirection(up_right), sq));
            }

            while (b2 != 0) {
                const sq = bb.popLsb(&b2);
                self.addMove(Move.normalMove(sq.subDirection(up_left), sq));
            }

            if (position.state_info.en_passant) |ep| {
                assert(if (us == .white) ep.rank() == 5 else ep.rank() == 2);

                if (move_gen_type == .evasions and (target & (bb.squareBB(ep.addDirection(up)))) != 0) {
                    return;
                }

                b1 = pawns_not_on_7 & bb.pawnAttacksBySquare(them, ep);

                while (b1 != 0) {
                    const sq = bb.popLsb(&b1);
                    self.addMove(Move.enPassantMove(sq, ep));
                }
            }
        }
    }

    fn generateMoves(self: *@This(), comptime piece_type: PieceType, comptime side: Color, comptime checks: bool, position: Position, target: Bitboard) void {
        comptime {
            assert(piece_type != .king and piece_type != .pawn);
        }

        var bitboard: Bitboard = position.piecesByColorAndType(side, piece_type);

        while (bitboard != 0) {
            const from = bb.popLsb(&bitboard);

            var b: Bitboard = bb.attacksBB(piece_type, from, position.pieces()) & target;

            if (checks and (piece_type == .queen or (position.blockersForKing(side.flip()) & bb.squareBB(from)) == 0)) {
                b &= position.checkSquare(piece_type);
            }

            while (b != 0) {
                self.addMove(Move.normalMove(from, bb.popLsb(&b)));
            }
        }
    }

    fn generateAll(self: *@This(), comptime move_gen_type: MoveGenType, comptime us: Color, position: Position) void {
        comptime {
            assert(move_gen_type != MoveGenType.legal);
        }

        const checks = comptime move_gen_type == .quiet_checks;
        const king_square = position.square(.king, us);
        var target: Bitboard = 0;

        if (move_gen_type != .evasions or !bb.moreThanOne(position.checkers())) {
            target = switch (move_gen_type) {
                .evasions => bb.betweenBB(king_square, @intToEnum(Square, @ctz(Bitboard, position.checkers()))),
                .non_evasions => ~position.piecesByColor(us),
                .captures => position.piecesByColor(us.flip()),
                else => ~position.allPieces(),
            };

            self.generatePawnMoves(move_gen_type, us, position, target);
            self.generateMoves(.knight, us, checks, position, target);
            self.generateMoves(.bishop, us, checks, position, target);
            self.generateMoves(.rook, us, checks, position, target);
            self.generateMoves(.queen, us, checks, position, target);
        }

        if (!checks or (position.blockersForKing(us.flip()) & bb.squareBB(king_square)) != 0) {
            var b = bb.pseudoAttacksBB(.king, king_square) & (if (move_gen_type == .evasions) ~position.piecesByColor(us) else target);
            if (checks) {
                b &= ~bb.pseudoAttacksBB(.queen, position.square(.king, us.flip()));
            }

            while (b != 0) {
                self.addMove(Move.normalMove(king_square, bb.popLsb(&b)));
            }

            if ((move_gen_type == .quiets or move_gen_type == .non_evasions) and position.canCastle(if (us == .white) .white_castling else .black_castling)) {
                const rights = if (us == .white) [_]types.CastlingRights{ .white_queen_side_castle, .white_king_side_castle }
                            else [_]types.CastlingRights{ .black_queen_side_castle, .black_king_side_castle };
                for (rights) |cr| {
                    if (!position.castlingImpeded(cr) and position.canCastle(cr)) {
                        self.addMove(Move.castlingMove(king_square, position.castlingRookSquare(cr)));
                    }
                }
            }
        }
    }
};
