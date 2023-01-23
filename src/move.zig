const std = @import("std");
const assert = std.debug.assert;

const types = @import("types.zig");
const Board = @import("board.zig").Board;
const bb = @import("bitboard.zig");
const Bitboard = bb.Bitboard;
const Color = types.Color;
const Square = types.Square;
const PieceType = types.PieceType;

pub const Move = packed struct {
    from: Square,
    to: Square,
    promotion: u2,
    specialBits: u2,

    pub fn normalMove(from: Square, to: Square) @This() {
        return .{
            .from = from,
            .to = to,
            .promotion = 0x0,
            .specialBits = 0x0,
        };
    }

    pub fn promotionMove(from: Square, to: Square, piece: PieceType) @This() {
        return .{
            .from = from,
            .to = to,
            .promotion = switch (piece) {
                PieceType.Knight => 0x0,
                PieceType.Bishop => 0x1,
                PieceType.Rook => 0x2,
                PieceType.Queen => 0x3,
                else => 0x0,
            },
            .specialBits = 0x1,
        };
    }

    pub fn enPassantMove(from: Square, to: Square) @This() {
        return .{
            .from = from,
            .to = to,
            .promotion = 0x0,
            .specialBits = 0x2,
        };
    }

    pub fn castlingMove(from: Square, to: Square) @This() {
        return .{
            .from = from,
            .to = to,
            .promotion = 0x0,
            .specialBits = 0x3,
        };
    }

    pub fn promotionToPiece(self: @This()) PieceType {
        return switch (self.promotion) {
            0 => PieceType.Knight,
            1 => PieceType.Bishop,
            2 => PieceType.Rook,
            3 => PieceType.Queen,
        };
    }

    pub fn isPromotion(self: @This()) bool {
        return self.specialBits == 0x1;
    }

    pub fn isEnPassant(self: @This()) bool {
        return self.specialBits == 0x2;
    }

    pub fn isCastling(self: @This()) bool {
        return self.specialBits == 0x3;
    }

    pub fn print(self: @This()) void {
        std.debug.print("from: {}, to: {}, promotion: {}, isPromotion: {}, isEnPassant: {}, isCastling: {}\n", .{
            self.from,
            self.to,
            self.promotionToPiece(),
            self.isPromotion(),
            self.isEnPassant(),
            self.isCastling(),
        });
    }
};

pub const ExtMove = struct {
    move: Move,
    value: i32,
};

pub const MoveList = struct {
    moves: std.ArrayList(ExtMove),

    pub fn print(self: @This()) void {
        std.debug.print("Size: {}\n", .{self.moves.items.len});
        for (self.moves.items) |extMove| {
            std.debug.print("{{ move: {}, value: {} }}\n", .{ extMove.move, extMove.value });
        }
    }
};

pub const MoveGenType = enum {
    Captures,
    Quiets,
    QuietChecks,
    Evasions,
    NonEvasions,
    Legal,
};

pub const MoveGenerator = struct {
    pub fn generatePawnMoves(comptime moveGenType: MoveGenType, comptime side: Color, board: Board, moveList: *MoveList, target: Bitboard) !void {
        const Them: Color = if (side == Color.White) Color.Black else Color.White;
        const TRank7BB: Bitboard = comptime if (side == Color.White) bb.Rank7BB else bb.Rank2BB;
        const TRank3BB: Bitboard = comptime if (side == Color.White) bb.Rank3BB else bb.Rank6BB;
        const Up: i6 = comptime if (side == Color.White) 8 else -8;
        const UpRight: i6 = comptime if (side == Color.White) 7 else -9;
        const UpLeft: i6 = comptime if (side == Color.White) 9 else -7;

        const pawnsOn7 = board.byColorBB[side.asKey()] & board.byTypeBB[PieceType.Pawn.asKey()] & TRank7BB;
        const pawnsNotOn7 = board.byColorBB[side.asKey()] & board.byTypeBB[PieceType.Pawn.asKey()] & ~TRank7BB;

        const enemies = if (moveGenType == MoveGenType.Evasions) board.byColorBB[Them.asKey()] & target else (if (moveGenType == MoveGenType.Captures) target else board.byColorBB[Them.asKey()]);

        var emptySquares: Bitboard = 0;

        moveList.moves.clearAndFree();

        if (moveGenType != MoveGenType.Captures) {
            emptySquares = if (moveGenType == MoveGenType.Quiets or moveGenType == MoveGenType.QuietChecks) target else ~board.byTypeBB[PieceType.AllPieces.asKey()];
            var b1: Bitboard = bb.shift(pawnsNotOn7, Up) & emptySquares;
            var b2: Bitboard = bb.shift((b1 & TRank3BB), Up) & emptySquares;

            if (moveGenType == MoveGenType.Evasions) {
                b1 &= target;
                b2 &= target;
            }

            if (moveGenType == MoveGenType.QuietChecks) {
                const kingSquare = board.byTypeBB[PieceType.King.asKey()] & board.byTypeBB[Them.asKey()];

                // TODO
                const dcCandidatePawns: Bitboard = 0;
                b1 &= bb.PreCalcBitboards.pawnAttacks[Them.asKey()][kingSquare.asKey()] | bb.shift(dcCandidatePawns, Up);
                b2 &= bb.PreCalcBitboards.pawnAttacks[Them.asKey()][kingSquare.asKey()] | bb.shift(dcCandidatePawns, Up + Up);
            }

            while (b1 != 0) {
                const to = @ctz(Bitboard, b1);
                const move = Move.normalMove(@intToEnum(Square, @intCast(u6, @intCast(i7, to) - Up)), @intToEnum(Square, to));
                try moveList.moves.append(.{
                    .move = move,
                    .value = 0,
                });
                b1 &= b1 - 1;
            }

            while (b2 != 0) {
                const to = @ctz(Bitboard, b2);
                const move = Move.normalMove(@intToEnum(Square, @intCast(u6, @intCast(i7, to) - Up - Up)), @intToEnum(Square, to));
                try moveList.moves.append(.{
                    .move = move,
                    .value = 0,
                });
                b2 &= b2 - 1;
            }
        }

        if (pawnsOn7 != 0) {
            if (moveGenType == MoveGenType.Captures) {
                emptySquares = ~board.byTypeBB[PieceType.AllPieces.asKey()];
            }

            if (moveGenType == MoveGenType.Evasions) {
                emptySquares &= target;
            }

            var b1: Bitboard = bb.shift(pawnsOn7, UpRight) & bb.NoHFile & enemies;
            var b2: Bitboard = bb.shift(pawnsOn7, UpLeft) & bb.NoAFile & enemies;
            var b3: Bitboard = bb.shift(pawnsOn7, Up) & emptySquares;

            while (b1 != 0) {
                const to = @ctz(Bitboard, b1);
                try makePromotionMoves(moveGenType, @intCast(u6, @intCast(i7, to) - UpRight), @intCast(u6, to), moveList);
                b1 &= b1 - 1;
            }

            while (b2 != 0) {
                const to = @ctz(Bitboard, b2);
                try makePromotionMoves(moveGenType, @intCast(u6, @intCast(i7, to) - UpLeft), @intCast(u6, to), moveList);
                b2 &= b2 - 1;
            }

            while (b3 != 0) {
                const to = @ctz(Bitboard, b3);
                try makePromotionMoves(moveGenType, @intCast(u6, @intCast(i7, to) - Up), @intCast(u6, to), moveList);
                b3 &= b3 - 1;
            }
        }

        if (moveGenType == MoveGenType.Captures or moveGenType == MoveGenType.Evasions or moveGenType == MoveGenType.NonEvasions) {
            var b1 = bb.shift(pawnsNotOn7, UpRight) & bb.NoHFile & enemies;
            var b2 = bb.shift(pawnsNotOn7, UpLeft) & bb.NoHFile & enemies;

            while (b1 != 0) {
                const to = @ctz(Bitboard, b1);
                const move = Move.normalMove(@intToEnum(Square, @intCast(u6, @intCast(i7, to) - UpRight)), @intToEnum(Square, to));
                try moveList.moves.append(.{
                    .move = move,
                    .value = 0,
                });
                b1 &= b1 - 1;
            }

            while (b2 != 0) {
                const to = @ctz(Bitboard, b2);
                const move = Move.normalMove(@intToEnum(Square, @intCast(u6, @intCast(i7, to) - UpLeft)), @intToEnum(Square, to));
                try moveList.moves.append(.{
                    .move = move,
                    .value = 0,
                });
                b2 &= b2 - 1;
            }

            if (board.enPassant) |ep| {
                assert(if (side == Color.White) ep.rank() == 5 else ep.rank() == 2);

                if (moveGenType == MoveGenType.Evasions and !(target & (ep.toBitboard << Up))) {
                    return;
                }

                b1 = pawnsNotOn7 & bb.PreCalcBitboards.pawnAttacks[Them.asKey()][ep.asKey()];

                while (b1 != 0) {
                    const from = @ctz(Bitboard, b1);
                    try makeEnPassantMove(@intCast(u6, from), ep, moveList);
                    b1 &= b1 - 1;
                }
            }
        }
    }

    fn makePromotionMoves(comptime pieceType: MoveGenType, from: u6, to: u6, moveList: *MoveList) !void {
        if (pieceType == MoveGenType.Captures or pieceType == MoveGenType.Evasions or pieceType == MoveGenType.NonEvasions) {
            const moveQueen = Move.promotionMove(@intToEnum(Square, from), @intToEnum(Square, to), PieceType.Queen);
            try moveList.moves.append(.{
                .move = moveQueen,
                .value = 0,
            });
        }
        if (pieceType == MoveGenType.Quiets or pieceType == MoveGenType.Evasions or pieceType == MoveGenType.NonEvasions) {
            const moveRook = Move.promotionMove(@intToEnum(Square, from), @intToEnum(Square, to), PieceType.Rook);
            try moveList.moves.append(.{
                .move = moveRook,
                .value = 0,
            });

            const moveBishop = Move.promotionMove(@intToEnum(Square, from), @intToEnum(Square, to), PieceType.Bishop);
            try moveList.moves.append(.{
                .move = moveBishop,
                .value = 0,
            });

            const moveKnight = Move.promotionMove(@intToEnum(Square, from), @intToEnum(Square, to), PieceType.Knight);
            try moveList.moves.append(.{
                .move = moveKnight,
                .value = 0,
            });
        }
    }

    fn makeEnPassantMove(from: u6, to: Square, moveList: *MoveList) !void {
        const move = Move.enPassantMove(@intToEnum(Square, from), to);
        try moveList.moves.append(.{
            .move = move,
            .value = 0,
        });
    }

    pub fn generateMoves(comptime pieceType: PieceType, comptime side: Color, comptime checks: bool, board: Board, moveList: *MoveList, target: Bitboard) !void {
        comptime {
            assert(pieceType != PieceType.King and pieceType != PieceType.Pawn);
        }

        var bitboard: Bitboard = board.byColorBB[side.asKey()] & board.byTypeBB[pieceType.asKey()];

        while (bitboard != 0) {
            const from = @intToEnum(Square, @ctz(Bitboard, bitboard));
            var b = bb.PreCalcBitboards.attacksBB(pieceType, from, board.byTypeBB[PieceType.AllPieces.asKey()]) & target;

            // TODO
            // if (checks && (pieceType == PieceType.Queen or
            _ = checks;

            while (b != 0) {
                const to = @intToEnum(Square, @ctz(Bitboard, b));
                const move = Move.normalMove(from, to);
                try moveList.moves.append(.{
                    .move = move,
                    .value = 0,
                });
                b &= b - 1;
            }

            bitboard &= bitboard - 1;
        }
    }

    pub fn generateAll(comptime moveGenType: MoveGenType, comptime side: Color, board: Board, moveList: *MoveList) !void {
        comptime {
            assert(moveGenType != MoveGenType.Legal);
        }

        const Checks = comptime moveGenType == MoveGenType.QuietChecks;
        const ksq = @intToEnum(Square, @ctz(Bitboard, board.byTypeBB[PieceType.King.asKey()] & board.byColorBB[side.asKey()]));
        var target: Bitboard = 0;
        _ = Checks;
        _ = ksq;
        _ = target;
        _ = moveList;

        // TODO skip generating king moves when in double check
    }
};
