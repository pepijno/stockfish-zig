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
                PieceType.knight => 0x0,
                PieceType.bishop => 0x1,
                PieceType.rook => 0x2,
                PieceType.queen => 0x3,
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
            0 => PieceType.knight,
            1 => PieceType.bishop,
            2 => PieceType.rook,
            3 => PieceType.queen,
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
