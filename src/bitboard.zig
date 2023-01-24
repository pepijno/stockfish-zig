const std = @import("std");

const types = @import("types.zig");
const Square = types.Square;
const Color = types.Color;
const PieceType = types.PieceType;
const Direction = types.Direction;
const Random = @import("random.zig").Random;

pub const Bitboard = u64;

pub const AllSquares: Bitboard = ~Bitboard(0);
pub const DarkSquares: Bitboard = 0xAA55AA55AA55AA55;

pub const Rank1BB: Bitboard = 0xFF;
pub const Rank2BB: Bitboard = Rank1BB << (1 * types.N_FILE);
pub const Rank3BB: Bitboard = Rank1BB << (2 * types.N_FILE);
pub const Rank4BB: Bitboard = Rank1BB << (3 * types.N_FILE);
pub const Rank5BB: Bitboard = Rank1BB << (4 * types.N_FILE);
pub const Rank6BB: Bitboard = Rank1BB << (5 * types.N_FILE);
pub const Rank7BB: Bitboard = Rank1BB << (6 * types.N_FILE);
pub const Rank8BB: Bitboard = Rank1BB << (7 * types.N_FILE);

pub const FileABB: Bitboard = 0x0101010101010101;
pub const FileBBB: Bitboard = FileABB << 1;
pub const FileCBB: Bitboard = FileABB << 2;
pub const FileDBB: Bitboard = FileABB << 3;
pub const FileEBB: Bitboard = FileABB << 4;
pub const FileFBB: Bitboard = FileABB << 5;
pub const FileGBB: Bitboard = FileABB << 6;
pub const FileHBB: Bitboard = FileABB << 7;

// zig fmt: off
pub const QueenSide: Bitboard   = FileABB | FileBBB | FileCBB | FileDBB;
pub const CenterFiles: Bitboard = FileCBB | FileDBB | FileEBB | FileFBB;
pub const KingSide: Bitboard    = FileEBB | FileFBB | FileGBB | FileHBB;
pub const Center: Bitboard      = (FileDBB | FileEBB) & (Rank4BB | Rank5BB);

pub const KingFlank: [types.N_FILE]Bitboard = [_]Bitboard {
    QueenSide ^ FileDBB, QueenSide, QueenSide,
    CenterFiles, CenterFiles,
    KingSide, KingSide, KingSide ^ FileEBB,
};
// zig fmt: on

pub const SquareDistance = blk: {
    @setEvalBranchQuota(100_000);
    var table: [types.N_SQUARES][types.N_SQUARES]u8 = undefined;
    for (std.enums.values(Square)) |s1| {
        for (std.enums.values(Square)) |s2| {
            table[@enumToInt(s1)][@enumToInt(s2)] = @maximum(distance(types.File, s1, s2), distance(types.Rank, s1, s2));
        }
    }
    break :blk table;
};

pub const SquareBB = blk: {
    var table: [types.N_SQUARES]Bitboard = undefined;
    for (std.enums.values(Square)) |square| {
        table[@enumToInt(square)] = @as(Bitboard, 1) << @enumToInt(square);
    }
    break :blk table;
};

pub const PawnAttacks = blk: {
    var table: [types.N_COLORS][types.N_SQUARES]Bitboard = undefined;
    for (std.enums.values(Square)) |square| {
        table[@enumToInt(Color.white)][@enumToInt(square)] = pawnAttacksByBitboard(Color.white, squareBB(square));
        table[@enumToInt(Color.black)][@enumToInt(square)] = pawnAttacksByBitboard(Color.black, squareBB(square));
    }
    break :blk table;
};

pub var BetweenBB: [types.N_SQUARES][types.N_SQUARES]Bitboard = undefined;
pub var LineBB: [types.N_SQUARES][types.N_SQUARES]Bitboard = undefined;
pub var PseudoAttacks: [types.N_PIECE_TYPES][types.N_SQUARES]Bitboard = undefined;

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

pub var RookMagics: [types.N_SQUARES]Magic = undefined;
pub var BishopMagics: [types.N_SQUARES]Magic = undefined;

pub inline fn squareBB(square: Square) Bitboard {
    return SquareBB[@enumToInt(square)];
}

pub inline fn moreThanOne(bitboard: Bitboard) bool {
    return bitboard & (bitboard - 1);
}

pub inline fn oppositeColors(square1: Square, square2: Square) bool {
    return (@enumToInt(square1) + square1.rank() + @enumToInt(square2) + square2.rank()) & 1;
}

pub inline fn rankBB(rank: types.Rank) Bitboard {
    return Rank1BB << (8 * @enumToInt(rank));
}

pub inline fn squareRankBB(square: types.Square) Bitboard {
    return Rank1BB << (8 * square.rank());
}

pub inline fn fileBB(file: types.File) Bitboard {
    return FileABB << @enumToInt(file);
}

pub inline fn squareFileBB(square: types.Square) Bitboard {
    return FileABB << square.file();
}

pub inline fn shift(comptime direction: Direction, bitboard: Bitboard) Bitboard {
    // zig fmt: off
    return switch (direction) {
        Direction.north,     Direction.north_sorth => bitboard << @enumToInt(direction),
        Direction.south,     Direction.south_south => bitboard >> -@enumToInt(direction),
        Direction.east,      Direction.north_east  => (bitboard << @enumToInt(direction)) & ~FileHBB,
        Direction.south_east                       => (bitboard >> -@enumToInt(direction)) & ~FileHBB,
        Direction.north_west                       => (bitboard << @enumToInt(direction)) & ~FileABB,
        Direction.west,      Direction.south_west  => (bitboard >> -@enumToInt(direction)) & ~FileABB,
    };
    // zig fmt: on
}

pub inline fn pawnAttacksByBitboard(comptime color: Color, bb: Bitboard) Bitboard {
    if (color == Color.white) {
        return shift(Direction.north_west, bb) | shift(Direction.north_east, bb);
    } else {
        return shift(Direction.south_west, bb) | shift(Direction.south_east, bb);
    }
}

pub inline fn pawnAttacksBySquare(comptime color: Color, square: Square) Bitboard {
    return PawnAttacks[@enumToInt(color)][@enumToInt(square)];
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
    return LineBB[@enumToInt(square1)][@enumToInt(square2)];
}

pub inline fn betweenBB(square1: Square, square2: Square) Bitboard {
    return BetweenBB[@enumToInt(square1)][@enumToInt(square2)];
}

pub inline fn forwardRanksBB(color: Color, square: Square) Bitboard {
    if (color == Color.white) {
        return ~Rank1BB << 8 * types.relativeRank(Color.white, square);
    } else {
        return ~Rank8BB >> 8 * types.relativeRank(Color.black, square);
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
    return lineBB(square1, square2) & (Bitboard(1) << squareBB(square3));
}

pub inline fn distance(comptime T: type, x: Square, y: Square) u8 {
    if (T == types.File) {
        return std.math.absInt(@as(i8, x.file()) - @as(i8, y.file())) catch unreachable;
    } else if (T == types.Rank) {
        return std.math.absInt(@as(i8, x.rank()) - @as(i8, y.rank())) catch unreachable;
    } else if (T == Square) {
        return SquareDistance[@enumToInt(x)][@enumToInt(y)];
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

pub inline fn attacksBB(comptime pieceType: PieceType, square: Square, occupied: Bitboard) Bitboard {
    const sq = @enumToInt(square);
    return switch (pieceType) {
        PieceType.bishop => BishopMagics[sq].attacks[BishopMagics[sq].index(occupied)],
        PieceType.rook => RookMagics[sq].attacks[RookMagics[sq].index(occupied)],
        PieceType.queen => attacksBB(PieceType.bishop, square, occupied) | attacksBB(PieceType.rook, square, occupied),
        else => PseudoAttacks[@enumToInt(pieceType)][sq],
    };
}

pub fn attacksBBRuntime(pieceType: PieceType, square: Square, occupied: Bitboard) Bitboard {
    const sq = @enumToInt(square);
    return switch (pieceType) {
        PieceType.bishop => attacksBB(PieceType.bishop, square, occupied),
        PieceType.rook => attacksBB(PieceType.rook, square, occupied),
        PieceType.queen => attacksBB(PieceType.bishop, square, occupied) | attacksBB(PieceType.rook, square, occupied),
        else => PseudoAttacks[@enumToInt(pieceType)][sq],
    };
}

pub inline fn leastSignificantSquareBB(bb: Bitboard) Bitboard {
    return bb & -bb;
}

pub inline fn popLsb(bb: *Bitboard) Square {
    const square = @intToEnum(Square, @ctz(Bitboard, bb));
    bb &= bb - 1;
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

    const newSquare: Square = @intToEnum(Square, @intCast(u6, new));
    return if (distance(Square, square, newSquare) <= 2) squareBB(newSquare) else 0;
}

var rookTable: [0x19000]Bitboard = undefined;
var bishopTable: [0x1480]Bitboard = undefined;

pub fn init() void {
    initMagics(PieceType.bishop, bishopTable[0..], BishopMagics[0..]);
    initMagics(PieceType.rook, rookTable[0..], RookMagics[0..]);

    for (std.enums.values(Square)) |square| {
        PseudoAttacks[@enumToInt(PieceType.king)][@enumToInt(square)] = 0;
        for ([_]i8{ -9, -8, -7, -1, 1, 7, 8, 9 }) |step| {
            PseudoAttacks[@enumToInt(PieceType.king)][@enumToInt(square)] |= safeDestination(square, step);
        }

        PseudoAttacks[@enumToInt(PieceType.knight)][@enumToInt(square)] = 0;
        for ([_]i8{ -17, -15, -10, -6, 6, 10, 15, 17 }) |step| {
            PseudoAttacks[@enumToInt(PieceType.knight)][@enumToInt(square)] |= safeDestination(square, step);
        }

        PseudoAttacks[@enumToInt(PieceType.bishop)][@enumToInt(square)] = attacksBB(PieceType.bishop, square, 0);
        PseudoAttacks[@enumToInt(PieceType.rook)][@enumToInt(square)] = attacksBB(PieceType.rook, square, 0);
        PseudoAttacks[@enumToInt(PieceType.queen)][@enumToInt(square)] = attacksBB(PieceType.bishop, square, 0);
        PseudoAttacks[@enumToInt(PieceType.queen)][@enumToInt(square)] |= attacksBB(PieceType.rook, square, 0);

        inline for ([_]PieceType{ PieceType.bishop, PieceType.rook }) |piece_type| {
            for (std.enums.values(Square)) |sq| {
                if (PseudoAttacks[@enumToInt(piece_type)][@enumToInt(square)] & squareBB(sq) != 0) {
                    LineBB[@enumToInt(square)][@enumToInt(sq)] = (attacksBB(piece_type, square, 0) & attacksBB(piece_type, sq, 0)) | squareBB(square) | squareBB(sq);
                    BetweenBB[@enumToInt(square)][@enumToInt(sq)] = (attacksBB(piece_type, square, squareBB(sq)) & attacksBB(piece_type, sq, squareBB(square)));
                }
                BetweenBB[@enumToInt(square)][@enumToInt(sq)] |= squareBB(sq);
            }
        }
    }
}

fn slidingAttack(comptime piece_type: PieceType, square: Square, occupied: Bitboard) Bitboard {
    const RookDirections = [_]Direction{Direction.north, Direction.south, Direction.east, Direction.west};
    const BishopDirections = [_]Direction{Direction.north_east, Direction.south_east, Direction.south_west, Direction.north_west};

    const directions = comptime if (piece_type == PieceType.rook) RookDirections else BishopDirections;

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
    const seeds = [2][types.N_RANK]u64{
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
        const edges: Bitboard = ((Rank1BB | Rank8BB) & ~(squareRankBB(square))) | ((FileABB | FileHBB) & ~(squareFileBB(square)));

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
    var rank: usize = types.N_RANK;
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
