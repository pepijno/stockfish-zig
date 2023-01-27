const std = @import("std");
const assert = std.debug.assert;

const types = @import("types.zig");
const bb = @import("bitboard.zig");
const Bitboard = bb.Bitboard;
const Color = types.Color;
const Square = types.Square;
const PieceType = types.PieceType;

pub const MoveType = enum(u2) {
    normal,
    promotion,
    en_passant,
    castling,
};

pub const Move = packed struct {
    from: Square,
    to: Square,
    promotion: u2,
    move_type: MoveType,

    pub fn normalMove(from: Square, to: Square) @This() {
        return .{
            .from = from,
            .to = to,
            .promotion = 0x0,
            .move_type = MoveType.normal,
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
            .move_type = MoveType.promotion,
        };
    }

    pub fn enPassantMove(from: Square, to: Square) @This() {
        return .{
            .from = from,
            .to = to,
            .promotion = 0x0,
            .move_type = MoveType.en_passant,
        };
    }

    pub fn castlingMove(from: Square, to: Square) @This() {
        return .{
            .from = from,
            .to = to,
            .promotion = 0x0,
            .move_type = MoveType.castling,
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

    pub fn print(self: @This()) void {
        std.debug.print("from: {}, to: {}, promotion: {}, move_type: {}\n", .{
            self.from,
            self.to,
            self.promotionToPiece(),
            self.move_type
        });
    }

    pub fn toString(self: @This(), allocator: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{{from: {s}, to: {s}, promotion: {}, move_type: {}}}", .{self.from.toString(allocator), self.to.toString(allocator), self.promotionToPiece(), self.move_type});
    }
};
