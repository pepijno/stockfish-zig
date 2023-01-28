const std = @import("std");

pub const max_ply: usize = 246;

pub const Rank = enum(u6) {
    // zig fmt: off
    rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8,
    // zig fmt: on
};
pub const n_rank = std.enums.values(Rank).len;

pub const File = enum(u6) {
    // zig fmt: off
    a, b, c, d, e, f, g, h,
    // zig fmt: on
};
pub const n_file = std.enums.values(Rank).len;

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

    pub inline fn addDirection(self: @This(), direction: Direction) Square {
        return @intToEnum(Square, @intCast(u6, @as(i8, @enumToInt(self)) + @enumToInt(direction)));
    }

    pub inline fn subDirection(self: @This(), direction: Direction) Square {
        return @intToEnum(Square, @intCast(u6, @as(i8, @enumToInt(self)) - @enumToInt(direction)));
    }

    pub inline fn toString(self: @This(), allocator: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{c}{c}", .{'a' + @as(u8, self.file()), '1' + @as(u8, self.rank())});
    }
};
pub const n_squares = std.enums.values(Square).len;

pub inline fn makeSquare(file: File, rank: Rank) Square {
    return @intToEnum(Square, n_file * @enumToInt(rank) + @enumToInt(file));
}

pub inline fn relativeRankBySquare(color: Color, square: Square) Rank {
    return relativeRank(color, @intToEnum(Rank, square.rank()));
}

pub inline fn relativeRank(color: Color, rank: Rank) Rank {
    return @intToEnum(Rank, @enumToInt(rank) ^ (@as(u8, @enumToInt(color)) * 7));
}

pub inline fn relativeSquare(color: Color, square: Square) Square {
    return @intToEnum(Square, @enumToInt(square) ^ (@as(u8, @enumToInt(color)) * 56));
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

    pub fn flip(self: @This()) Color {
        return if (self == Color.white) Color.black else Color.white;
    }
};
pub const n_colors = std.enums.values(Color).len;

pub inline fn pawnPush(color: Color) Direction {
    return if (color == Color.white) Direction.north else Direction.south;
}

pub const PieceType = enum {
    no_piece_type,
    pawn,
    knight,
    bishop,
    rook,
    queen,
    king,
    all_pieces,
};
pub const n_piece_types = std.enums.values(PieceType).len;

pub const Piece = enum(u4) {
    no_piece,
    white_pawn = 1,
    white_knight = 2,
    white_bishop = 3,
    white_rook = 4,
    white_queen = 5,
    white_king = 6,
    black_pawn = 9,
    black_knight = 10,
    black_bishop = 11,
    black_rook = 12,
    black_queen = 13,
    black_king = 14,
    _,

    pub inline fn typeOf(self: @This()) PieceType {
        return @intToEnum(PieceType, @enumToInt(self) & 7);
    }

    pub inline fn colorOf(self: @This()) Color {
        return @intToEnum(Color, @enumToInt(self) >> 3);
    }
};
pub const n_pieces = 16;

pub fn makePiece(color: Color, piece_type: PieceType) Piece {
    return @intToEnum(Piece, (@as(u6, @enumToInt(color)) << 3) + @enumToInt(piece_type));
}

// zig fmt: off
pub const pieces_table = [_]Piece{ Piece.white_pawn, Piece.white_knight, Piece.white_bishop, Piece.white_rook, Piece.white_queen, Piece.white_king,
                                   Piece.black_pawn, Piece.black_knight, Piece.black_bishop, Piece.black_rook, Piece.black_queen, Piece.black_king };
// zig fmt: on

pub const n_castling_rights = 16;
pub const CastlingType = u4;
pub const CastlingRights = enum(u4) {
    no_castling = 0,
    white_king_side_castle = 1,
    white_queen_side_castle = 2,
    black_king_side_castle = 4,
    black_queen_side_castle = 8,

    king_side = 1 | 4,
    queen_side = 2 | 8,
    white_castling = 1 | 2,
    black_castling = 4 | 8,
    any_castling = 1 | 2 | 4 | 8,
    _,

    pub fn updateCastlingRights(self: @This(), castling_rights_mask: [n_squares]u4, square: Square) CastlingRights {
        var rights = @enumToInt(self);
        rights &= ~castling_rights_mask[@enumToInt(square)];
        return @intToEnum(CastlingRights, rights);
    }

    pub fn addCastlingRights(self: @This(), cr: CastlingRights) CastlingRights {
        var rights = @enumToInt(self);
        rights |= @enumToInt(cr);
        return @intToEnum(CastlingRights, rights);
    }

    pub fn hasCastlingRights(self: @This(), cr: CastlingRights) bool {
        var rights = @enumToInt(self);
        return rights & @enumToInt(cr) != 0;
    }
};

pub const Value = enum(i32) {
    value_draw = 0,
    value_known_win = 10000,
    value_mate = 32000,
    value_infinite = 32001,
    value_none = 32002,

    value_tb_win_in_max_ply = 32000 - 2 * max_ply,
    value_tb_loss_in_max_ply = @as(i32, -32000) + 2 * max_ply,
    value_mate_in_max_ply = 32000 - max_ply,
    value_mated_in_max_ply = @as(i32, -32000) + max_ply,

    pawn_value_mg = 126,
    pawn_value_eg = 208,
    knight_value_mg = 781,
    knight_value_eg = 854,
    bishop_value_mg = 825,
    bishop_value_eg = 915,
    rook_value_mg = 1275,
    rook_value_eg = 1380,
    queen_value_mg = 2358,
    queen_value_eg = 2682,

    midgame_limit = 15258,
    endgame_limit = 3915,
    _,

    pub fn add(self: @This(), value: @This()) @This() {
        return @intToEnum(@This(), @enumToInt(self) + @enumToInt(value));
    }

    pub fn sub(self: @This(), value: @This()) @This() {
        return @intToEnum(@This(), @enumToInt(self) - @enumToInt(value));
    }
};

pub const Phase = enum(u16) {
    mg = 0,
    eg = 1,
    phase_midgame = 128,
    _,
};
pub const n_phases = 2;

pub const piece_value = [n_phases][n_pieces]Value {
    [_]Value{ Value.value_draw, Value.pawn_value_mg, Value.knight_value_mg, Value.bishop_value_mg, Value.rook_value_mg, Value.queen_value_mg, Value.value_draw, Value.value_draw,
              Value.value_draw, Value.pawn_value_mg, Value.knight_value_mg, Value.bishop_value_mg, Value.rook_value_mg, Value.queen_value_mg, Value.value_draw, Value.value_draw },
    [_]Value{ Value.value_draw, Value.pawn_value_eg, Value.knight_value_eg, Value.bishop_value_eg, Value.rook_value_eg, Value.queen_value_eg, Value.value_draw, Value.value_draw,
              Value.value_draw, Value.pawn_value_eg, Value.knight_value_eg, Value.bishop_value_eg, Value.rook_value_eg, Value.queen_value_eg, Value.value_draw, Value.value_draw },
};

pub const Score = enum(i32) {
    zero_score,
    _,

    pub fn add(self: @This(), score: @This()) @This() {
        return @intToEnum(@This(), @enumToInt(self) + @enumToInt(score));
    }

    pub fn sub(self: @This(), score: @This()) @This() {
        return @intToEnum(@This(), @enumToInt(self) - @enumToInt(score));
    }
};

pub fn makeScore(mg: i32, eg: i32) Score {
    return @intToEnum(Score, @bitCast(i32, @bitCast(u32, eg) << 16) + mg);
}

pub fn egValue(psq: Score) Value {
    const Eg = union {
        u: u16,
        s: i16,
    };
    const eg = Eg{ .u = u16(u32(psq + 0x8000) >> 16) };
    return eg.s;
}

pub inline fn mulHi64(a: u64, b: u64) u64 {
    return @intCast(u64, (@as(u128, a) * @as(u128, b)) >> 64);
}

pub const Depth = i32;

pub const Bound = enum(u4) {
    bound_none = 0,
    boune_upper = 1,
    bound_lower = 2,
    bound_exact = 3,
};

pub const depth_offset: i32 = -7;
