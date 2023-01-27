const std = @import("std");
const assert = std.debug.assert;

const types = @import("types.zig");
const Square = types.Square;
const Color = types.Color;
const PieceType = types.PieceType;
const Direction = types.Direction;
const Random = @import("random.zig").Random;

pub const Bitboard = u64;

pub const all_squares: Bitboard = ~Bitboard(0);
pub const dark_squares: Bitboard = 0xAA55AA55AA55AA55;

pub const rank_1_bb: Bitboard = 0xFF;
pub const rank_2_bb: Bitboard = rank_1_bb << (1 * types.n_file);
pub const rank_3_bb: Bitboard = rank_1_bb << (2 * types.n_file);
pub const rank_4_bb: Bitboard = rank_1_bb << (3 * types.n_file);
pub const rank_5_bb: Bitboard = rank_1_bb << (4 * types.n_file);
pub const rank_6_bb: Bitboard = rank_1_bb << (5 * types.n_file);
pub const rank_7_bb: Bitboard = rank_1_bb << (6 * types.n_file);
pub const rank_8_bb: Bitboard = rank_1_bb << (7 * types.n_file);

pub const file_a_bb: Bitboard = 0x0101010101010101;
pub const file_b_bb: Bitboard = file_a_bb << 1;
pub const file_c_bb: Bitboard = file_a_bb << 2;
pub const file_d_bb: Bitboard = file_a_bb << 3;
pub const file_e_bb: Bitboard = file_a_bb << 4;
pub const file_f_bb: Bitboard = file_a_bb << 5;
pub const file_g_bb: Bitboard = file_a_bb << 6;
pub const file_h_bb: Bitboard = file_a_bb << 7;

// zig fmt: off
pub const queen_side: Bitboard   = file_a_bb | file_b_bb | file_c_bb | file_d_bb;
pub const center_files: Bitboard = file_c_bb | file_d_bb | file_e_bb | file_f_bb;
pub const king_side: Bitboard    = file_e_bb | file_f_bb | file_g_bb | file_h_bb;
pub const center: Bitboard      = (file_d_bb | file_e_bb) & (rank_4_bb | rank_5_bb);

pub const king_flank: [types.n_file]Bitboard = [_]Bitboard {
    queen_side ^ file_d_bb, queen_side, queen_side,
    center_files, center_files,
    king_side, king_side, king_side ^ file_e_bb,
};
// zig fmt: on

pub const square_distance = blk: {
    @setEvalBranchQuota(100_000);
    var table: [types.n_squares][types.n_squares]u8 = undefined;
    for (std.enums.values(Square)) |s1| {
        for (std.enums.values(Square)) |s2| {
            table[@enumToInt(s1)][@enumToInt(s2)] = @maximum(distance(types.File, s1, s2), distance(types.Rank, s1, s2));
        }
    }
    break :blk table;
};

pub const square_bb = blk: {
    var table: [types.n_squares]Bitboard = undefined;
    for (std.enums.values(Square)) |square| {
        table[@enumToInt(square)] = @as(Bitboard, 1) << @enumToInt(square);
    }
    break :blk table;
};

pub const pawn_attacks = blk: {
    var table: [types.n_colors][types.n_squares]Bitboard = undefined;
    for (std.enums.values(Square)) |square| {
        table[@enumToInt(Color.white)][@enumToInt(square)] = pawnAttacksByBitboard(Color.white, squareBB(square));
        table[@enumToInt(Color.black)][@enumToInt(square)] = pawnAttacksByBitboard(Color.black, squareBB(square));
    }
    break :blk table;
};

pub var between_bb: [types.n_squares][types.n_squares]Bitboard = undefined;
pub var line_bb: [types.n_squares][types.n_squares]Bitboard = undefined;
pub var pseude_attacks: [types.n_piece_types][types.n_squares]Bitboard = undefined;

const Magic = struct {
    mask: Bitboard,
    magic: Bitboard,
    attacks: []Bitboard,
    shift: u8,

    pub fn index(self: @This(), occupied: Bitboard) u64 {
        // if (HasPext) {
        //     return
        // }

        // if (Is64Bit) {
        if (self.shift > 63) {
            return 0;
        } else {
            return ((occupied & self.mask) *% self.magic) >> @intCast(u6, self.shift);
        }
        // } else {
        // }
    }
};

pub var rook_magics: [types.n_squares]Magic = undefined;
pub var bishop_magics: [types.n_squares]Magic = undefined;

pub inline fn squareBB(square: Square) Bitboard {
    return square_bb[@enumToInt(square)];
}

pub inline fn moreThanOne(bitboard: Bitboard) bool {
    return (bitboard & (bitboard - 1)) != 0;
}

pub inline fn oppositeColors(square1: Square, square2: Square) bool {
    return ((@enumToInt(square1) + square1.rank() + @enumToInt(square2) + square2.rank()) & 1) == 1;
}

pub inline fn rankBB(rank: types.Rank) Bitboard {
    return rank_1_bb << (8 * @enumToInt(rank));
}

pub inline fn squareRankBB(square: types.Square) Bitboard {
    return rank_1_bb << (8 * square.rank());
}

pub inline fn fileBB(file: types.File) Bitboard {
    return file_a_bb << @enumToInt(file);
}

pub inline fn squareFileBB(square: types.Square) Bitboard {
    return file_a_bb << square.file();
}

pub inline fn shift(comptime direction: Direction, bitboard: Bitboard) Bitboard {
    // zig fmt: off
    return switch (direction) {
        Direction.north,     Direction.north_north => bitboard << @enumToInt(direction),
        Direction.south,     Direction.south_south => bitboard >> -@enumToInt(direction),
        Direction.east,      Direction.north_east  => (bitboard << @enumToInt(direction)) & ~file_a_bb,
        Direction.south_east                       => (bitboard >> -@enumToInt(direction)) & ~file_a_bb,
        Direction.north_west                       => (bitboard << @enumToInt(direction)) & ~file_h_bb,
        Direction.west,      Direction.south_west  => (bitboard >> -@enumToInt(direction)) & ~file_h_bb,
    };
    // zig fmt: on
}

pub inline fn pawnAttacksByBitboard(color: Color, bb: Bitboard) Bitboard {
    if (color == Color.white) {
        return shift(Direction.north_west, bb) | shift(Direction.north_east, bb);
    } else {
        return shift(Direction.south_west, bb) | shift(Direction.south_east, bb);
    }
}

pub inline fn pawnAttacksBySquare(color: Color, square: Square) Bitboard {
    return pawn_attacks[@enumToInt(color)][@enumToInt(square)];
}

pub inline fn doublePawnAttacks(comptime color: Color, bb: Bitboard) Bitboard {
    if (color == Color.white) {
        return shift(Direction.north_west, bb) & shift(Direction.north_east, bb);
    } else {
        return shift(Direction.south_west, bb) & shift(Direction.south_east, bb);
    }
}

pub inline fn adjacentFiles(square: Square) Bitboard {
    return shift(Direction.east, squareFileBB(square)) | shift(Direction.west, squareFileBB(square));
}

pub inline fn lineBB(square1: Square, square2: Square) Bitboard {
    return line_bb[@enumToInt(square1)][@enumToInt(square2)];
}

pub inline fn betweenBB(square1: Square, square2: Square) Bitboard {
    return between_bb[@enumToInt(square1)][@enumToInt(square2)];
}

pub inline fn forwardRanksBB(color: Color, square: Square) Bitboard {
    if (color == Color.white) {
        return ~rank_1_bb << @intCast(u6, 8 * types.relativeRank(Color.white, square));
    } else {
        return ~rank_8_bb >> @intCast(u6, 8 * types.relativeRank(Color.black, square));
    }
}

pub inline fn forwardFilesBB(color: Color, square: Square) Bitboard {
    return forwardFilesBB(color, square) & squareFileBB(square);
}

pub inline fn pawnAttackSpan(color: Color, square: Square) Bitboard {
    return forwardRanksBB(color, square) & adjacentFiles(square);
}

pub inline fn passedPawnSpan(color: Color, square: Square) Bitboard {
    return pawnAttackSpan(color, square) | forwardFilesBB(color, square);
}

pub inline fn aligned(square1: Square, square2: Square, square3: Square) bool {
    return (lineBB(square1, square2) & squareBB(square3)) != 0;
}

pub inline fn distance(comptime T: type, x: Square, y: Square) u8 {
    if (T == types.File) {
        return std.math.absInt(@as(i8, x.file()) - @as(i8, y.file())) catch unreachable;
    } else if (T == types.Rank) {
        return std.math.absInt(@as(i8, x.rank()) - @as(i8, y.rank())) catch unreachable;
    } else if (T == Square) {
        return square_distance[@enumToInt(x)][@enumToInt(y)];
    } else {
        unreachable;
    }
}

pub inline fn fileEdgeDistance(file: u8) u8 {
    return @minimum(file, @enumToInt(types.File.h) - file);
}

pub inline fn rankEdgeDistance(rank: u8) u8 {
    return @minimum(rank, @enumToInt(types.File.h) - rank);
}

pub inline fn pseudoAttacksBB(piece_type: PieceType, square: Square) Bitboard {
    assert(piece_type != types.PieceType.pawn);
    return pseude_attacks[@enumToInt(piece_type)][@enumToInt(square)];
}

pub inline fn attacksBB(piece_type: PieceType, square: Square, occupied: Bitboard) Bitboard {
    const sq = @enumToInt(square);
    return switch (piece_type) {
        PieceType.bishop => bishop_magics[sq].attacks[bishop_magics[sq].index(occupied)],
        PieceType.rook => rook_magics[sq].attacks[rook_magics[sq].index(occupied)],
        PieceType.queen => attacksBB(PieceType.bishop, square, occupied) | attacksBB(PieceType.rook, square, occupied),
        else => pseude_attacks[@enumToInt(piece_type)][sq],
    };
}
//
// pub fn attacksBBRuntime(piece_type: PieceType, square: Square, occupied: Bitboard) Bitboard {
//     const sq = @enumToInt(square);
//     return switch (piece_type) {
//         PieceType.bishop => attacksBB(PieceType.bishop, square, occupied),
//         PieceType.rook => attacksBB(PieceType.rook, square, occupied),
//         PieceType.queen => attacksBB(PieceType.bishop, square, occupied) | attacksBB(PieceType.rook, square, occupied),
//         else => pseude_attacks[@enumToInt(piece_type)][sq],
//     };
// }

pub inline fn leastSignificantSquareBB(bb: Bitboard) Bitboard {
    return bb & -bb;
}

pub inline fn popLsb(bb: *Bitboard) Square {
    const square = @intToEnum(Square, @ctz(Bitboard, bb.*));
    bb.* &= bb.* - 1;
    return square;
}

pub inline fn frontMostSquare(color: Color, bb: Bitboard) Square {
    if (color == Color.white) {
        return @intToEnum(Square, 63 ^ @ctz(Bitboard, bb));
    } else {
        return @intToEnum(Square, @ctz(Bitboard, bb));
    }
}

pub inline fn safeDestination(square: Square, step: i8) Bitboard {
    const new = @as(i8, @enumToInt(square)) + step;
    if (new < 0 or new > 63) {
        return 0;
    }

    const new_square: Square = @intToEnum(Square, @intCast(u6, new));
    return if (distance(Square, square, new_square) <= 2) squareBB(new_square) else 0;
}

var rook_table: [0x19000]Bitboard = undefined;
var bishop_table: [0x1480]Bitboard = undefined;

pub fn init() void {
    initMagics(PieceType.bishop, bishop_table[0..], bishop_magics[0..]);
    initMagics(PieceType.rook, rook_table[0..], rook_magics[0..]);

    for (std.enums.values(Square)) |square| {
        pseude_attacks[@enumToInt(PieceType.king)][@enumToInt(square)] = 0;
        for ([_]i8{ -9, -8, -7, -1, 1, 7, 8, 9 }) |step| {
            pseude_attacks[@enumToInt(PieceType.king)][@enumToInt(square)] |= safeDestination(square, step);
        }

        pseude_attacks[@enumToInt(PieceType.knight)][@enumToInt(square)] = 0;
        for ([_]i8{ -17, -15, -10, -6, 6, 10, 15, 17 }) |step| {
            pseude_attacks[@enumToInt(PieceType.knight)][@enumToInt(square)] |= safeDestination(square, step);
        }

        pseude_attacks[@enumToInt(PieceType.bishop)][@enumToInt(square)] = attacksBB(PieceType.bishop, square, 0);
        pseude_attacks[@enumToInt(PieceType.rook)][@enumToInt(square)] = attacksBB(PieceType.rook, square, 0);
        pseude_attacks[@enumToInt(PieceType.queen)][@enumToInt(square)] = attacksBB(PieceType.bishop, square, 0);
        pseude_attacks[@enumToInt(PieceType.queen)][@enumToInt(square)] |= attacksBB(PieceType.rook, square, 0);

        inline for ([_]PieceType{ PieceType.bishop, PieceType.rook }) |piece_type| {
            for (std.enums.values(Square)) |sq| {
                if (pseude_attacks[@enumToInt(piece_type)][@enumToInt(square)] & squareBB(sq) != 0) {
                    line_bb[@enumToInt(square)][@enumToInt(sq)] = (attacksBB(piece_type, square, 0) & attacksBB(piece_type, sq, 0)) | squareBB(square) | squareBB(sq);
                    between_bb[@enumToInt(square)][@enumToInt(sq)] = (attacksBB(piece_type, square, squareBB(sq)) & attacksBB(piece_type, sq, squareBB(square)));
                }
                between_bb[@enumToInt(square)][@enumToInt(sq)] |= squareBB(sq);
            }
        }
    }
}

fn slidingAttack(comptime piece_type: PieceType, square: Square, occupied: Bitboard) Bitboard {
    const rook_directions = [_]Direction{Direction.north, Direction.south, Direction.east, Direction.west};
    const bishop_directions = [_]Direction{Direction.north_east, Direction.south_east, Direction.south_west, Direction.north_west};

    const directions = comptime if (piece_type == PieceType.rook) rook_directions else bishop_directions;

    var attacks: Bitboard = 0;
    for (directions) |dir| {
        var s = square;
        while (safeDestination(s, @enumToInt(dir)) != 0 and (occupied & squareBB(s)) == 0) {
            s = @intToEnum(Square, @enumToInt(s) + @enumToInt(dir));
            attacks |= squareBB(s);
        }
    }

    return attacks;
}

fn initMagics(comptime piece_type: PieceType, table: []Bitboard, magics: []Magic) void {
    // zig fmt: off
    const seeds = [2][types.n_rank]u64{
        [_]u64{ 8977, 44560, 54343, 38998, 5731, 92505, 104912, 17020 },
        [_]u64{ 728, 10316, 55013, 32803, 12281, 15100, 16645, 255 }
    };
    // zig fmt: on

    var occupancy: [4096]Bitboard = undefined;
    var reference: [4096]Bitboard = undefined;
    var epoch: [4096]i32 = [_]i32{0} ** 4096;
    var cnt: i32 = 0;
    var size: usize = 0;

    for (std.enums.values(Square)) |square| {
        const edges: Bitboard = ((rank_1_bb | rank_8_bb) & ~(squareRankBB(square))) | ((file_a_bb | file_h_bb) & ~(squareFileBB(square)));

        var magic: *Magic = &magics[@enumToInt(square)];
        magic.mask = slidingAttack(piece_type, square, 0) & ~edges;
        // magic.shift = (if (Is64Bit) 64 else 32) - @popCount(Bitboard, magic.mask);
        magic.shift = 64 - @popCount(Bitboard, magic.mask);

        magic.attacks = if (square == Square.a1) table else magics[@enumToInt(square) - 1].attacks[size..];

        var b: Bitboard = 0;
        size = 0;
        while (true) {
            occupancy[size] = b;
            reference[size] = slidingAttack(piece_type, square, b);

            // if (HasPext) {
            // }

            size += 1;
            b = (b -% magic.mask) & magic.mask;

            if (b == 0) {
                break;
            }
        }

        // if (HasPext) {
        //     continue;
        // }

        // var rng: Random = comptime Random.create(seeds[Is64Bit][square.rank()]);
        var rng: Random = Random.create(seeds[1][square.rank()]);

        var i: usize = 0;
        while (i < size) {
            magic.magic = 0;
            while (@popCount(Bitboard, (magic.magic *% magic.mask) >> 56) < 6) {
                magic.magic = rng.sparseRand();
            }

            i = 0;
            cnt += 1;
            while (i < size) : (i += 1) {
                const index: u64 = magic.index(occupancy[i]);

                if (epoch[index] < cnt) {
                    epoch[index] = cnt;
                    magic.attacks[index] = reference[i];
                } else if (magic.attacks[index] != reference[i]) {
                    break;
                }
            }
        }
    }
}

pub inline fn isSet(bb: Bitboard, square: Square) bool {
    return (bb & squareBB(square) != 0);
}

pub inline fn set(bb: *Bitboard, square: Square) void {
    bb.* = (bb.* | squareBB(square));
}

pub inline fn unset(bb: *Bitboard, square: Square) void {
    bb.* = (bb.* & ~(squareBB(square)));
}

pub fn pretty(bb: Bitboard) void {
    std.debug.print("+---+---+---+---+---+---+---+---+\n", .{});
    var rank: usize = types.n_rank;
    while (rank > 0) {
        rank -= 1;
        for (std.enums.values(types.File)) |file| {
            if (bb & (@as(Bitboard, 1) << @enumToInt(types.makeSquare(file, @intToEnum(types.Rank, rank)))) != 0) {
                std.debug.print("| X ", .{});
            } else {
                std.debug.print("|   ", .{});
            }
        }
        std.debug.print("| {}\n+---+---+---+---+---+---+---+---+\n", .{1 + rank});
    }
    std.debug.print("  a   b   c   d   e   f   g   h\n", .{});
}
