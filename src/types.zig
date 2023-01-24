const std = @import("std");

pub const Rank = enum(u6) {
    // zig fmt: off
    rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8,
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

    pub fn rank(self: @This()) u6 {
        return @intCast(u6, @enumToInt(self) >> 3);
    }

    pub fn file(self: @This()) u6 {
        return @intCast(u6, @enumToInt(self) & 7);
    }

    pub inline fn asKey(self: @This()) u6 {
        return @enumToInt(self);
    }
};
pub const N_SQUARES = std.enums.values(Square).len;

pub inline fn makeSquare(file: File, rank: Rank) Square {
    return @intToEnum(Square, N_FILE * @enumToInt(rank) + @enumToInt(file));
}

pub inline fn relativeRank(comptime color: Color, square: Square) u8 {
    return square.rank() ^ (@enumToInt(color) * 7);
}

pub const Direction = enum(i8) {
    north = 8,
    east = 1,
    south = -8,
    west = -1,

    north_north = 16,
    south_south = -16,

    north_east = 9,
    north_west = 7,
    south_east = -7,
    south_west = -9,
};

pub const Color = enum(u1) {
    white,
    black,
};
pub const N_COLORS = std.enums.values(Color).len;

pub const PieceType = enum {
    no_piece_type,
    pawn,
    knight,
    bishop,
    rook,
    queen,
    king,
    all_pieces,

    pub inline fn asKey(self: @This()) u6 {
        return @enumToInt(self);
    }
};
pub const N_PIECE_TYPES = std.enums.values(PieceType).len;

pub const Piece = enum {
    no_piece,
    white_pawn,
    white_knight,
    white_bishop,
    white_rook,
    white_queen,
    white_king,
    black_pawn,
    black_knight,
    black_bishop,
    black_rook,
    black_queen,
    black_king,

    pub inline fn asKey(self: @This()) u6 {
        return @enumToInt(self);
    }
};
pub const N_PIECES = std.enums.values(Piece).len;

pub const N_CASTLING_RIGHTS = 16;
pub const CastlingType = u4;
pub const CastlingRights = packed struct {
    white_king_side_castle: bool = false,
    white_queen_side_castle: bool = false,
    black_king_side_castle: bool = false,
    black_queen_side_castle: bool = false,

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
