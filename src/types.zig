const std = @import("std");

pub const Rank = enum(u6) {
    // zig fmt: off
    Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8,
    // zig fmt: on
};
pub const N_RANK = std.enums.values(Rank).len;

pub const File = enum(u6) {
    // zig fmt: off
    a, b, c, d, e, f, g, h,
    // zig fmt: on
};
pub const N_FILE = std.enums.values(Rank).len;

pub const SquareType = u6;
pub const Square = enum(SquareType) {
    // zig fmt: off
    a1, b1, c1, d1, e1,f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8,
    // zig fmt: on

    pub fn rank(self: @This()) u3 {
        return @intCast(u3, @enumToInt(self) / N_FILE);
    }

    pub fn file(self: @This()) u3 {
        return @intCast(u3, @enumToInt(self) % N_FILE);
    }

    pub fn toBitboard(self: @This()) u64 {
        return @as(u64, 1) << @enumToInt(self);
    }

    pub inline fn asKey(self: @This()) u6 {
        return @enumToInt(self);
    }
};
pub const N_SQUARES = std.enums.values(Square).len;

pub const Color = enum(u1) {
    White,
    Black,

    pub inline fn asKey(self: @This()) u6 {
        return @enumToInt(self);
    }
};
pub const N_COLORS = std.enums.values(Color).len;

pub const PieceType = enum {
    NoPieceType,
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    AllPieces,

    pub inline fn asKey(self: @This()) u6 {
        return @enumToInt(self);
    }
};
pub const N_PIECE_TYPES = std.enums.values(PieceType).len;

pub const Piece = enum {
    NoPiece,
    WhitePawn,
    WhiteKnight,
    WhiteBishop,
    WhiteRook,
    WhiteQueen,
    WhiteKing,
    BlackPawn,
    BlackKnight,
    BlackBishop,
    BlackRook,
    BlackQueen,
    BlackKing,

    pub inline fn asKey(self: @This()) u6 {
        return @enumToInt(self);
    }
};
pub const N_PIECES = std.enums.values(Piece).len;

pub const N_CASTLING_RIGHTS = 16;
pub const CastlingType = u4;
pub const CastlingRights = packed struct {
    WhiteKingSideCastle: bool = false,
    WhiteQueenSideCastle: bool = false,
    BlackKingSideCastle: bool = false,
    BlackQueenSideCastle: bool = false,

    const CastlingRightsMask: [64]CastlingType = .{
        0x7, 0xF, 0xF, 0xF, 0x3, 0xF, 0xF, 0xB,
        0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF,
        0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF,
        0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF,
        0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF,
        0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF,
        0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF, 0xF,
        0xD, 0xF, 0xF, 0xF, 0xC, 0xF, 0xF, 0xE,
    };

    pub fn updateCastlingRights(self: *@This(), square: Square) void {
        var rights = @bitCast(CastlingType, self.*);
        rights &= CastlingRightsMask[@enumToInt(square)];
        self.* = @bitCast(CastlingRights, rights);
    }
};
