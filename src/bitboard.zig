const std = @import("std");
const assert = std.debug.assert;

const types = @import("types.zig");
const Square = types.Square;
const Color = types.Color;
const PieceType = types.PieceType;
const Random = @import("random.zig").Random;

pub const Bitboard = u64;

pub const NoAFile: Bitboard = 0xFEFEFEFEFEFEFEFE;
pub const NoHFile: Bitboard = 0x7F7F7F7F7F7F7F7F;

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

pub var PreCalcBitboards = PreCalc.create();

const PreCalc = struct {
    pawnAttacks: [types.N_COLORS][types.N_SQUARES]Bitboard,
    pseudoAttacks: [types.N_PIECE_TYPES][types.N_SQUARES]Bitboard,
    rookMagics: [types.N_SQUARES]Magic,
    bishopMagics: [types.N_SQUARES]Magic,

    pub fn init(self: *@This()) void {
        var rookTable: [0x19000]Bitboard = undefined;
        var bishopTable: [0x1480]Bitboard = undefined;
        initMagics(PieceType.Bishop, bishopTable[0..], self.bishopMagics[0..]);
        initMagics(PieceType.Rook, rookTable[0..], self.rookMagics[0..]);

        for (std.enums.values(Square)) |square| {
            self.pawnAttacks[@enumToInt(Color.White)][@enumToInt(square)] = pawnAttacksBitboard(Color.White, square.toBitboard());
            self.pawnAttacks[@enumToInt(Color.Black)][@enumToInt(square)] = pawnAttacksBitboard(Color.Black, square.toBitboard());
        }

        for (std.enums.values(Square)) |square| {
            self.pseudoAttacks[@enumToInt(PieceType.King)][@enumToInt(square)] = 0;
            for ([_]i8{ -9, -8, -7, -1, 1, 7, 8, 9 }) |step| {
                self.pseudoAttacks[@enumToInt(PieceType.King)][@enumToInt(square)] |= safeSquare(square, step);
            }

            self.pseudoAttacks[@enumToInt(PieceType.Knight)][@enumToInt(square)] = 0;
            for ([_]i8{ -17, -15, -10, -6, 6, 10, 15, 17 }) |step| {
                self.pseudoAttacks[@enumToInt(PieceType.Knight)][@enumToInt(square)] |= safeSquare(square, step);
            }

            self.pseudoAttacks[@enumToInt(PieceType.Rook)][@enumToInt(square)] = self.attacksBB(PieceType.Rook, square, 0);
            self.pseudoAttacks[@enumToInt(PieceType.Bishop)][@enumToInt(square)] = self.attacksBB(PieceType.Bishop, square, 0);
            self.pseudoAttacks[@enumToInt(PieceType.Queen)][@enumToInt(square)] = self.attacksBB(PieceType.Rook, square, 0);
            self.pseudoAttacks[@enumToInt(PieceType.Queen)][@enumToInt(square)] |= self.attacksBB(PieceType.Bishop, square, 0);

            // for ([_]PieceType{ PieceType.Bishop, PieceType.Rook }) |pieceType| {
            //     for (std.enums.values(Square)) |square2| {
            //         if (self.pseudoAttacks[@enumToInt(pieceType)][@enumToInt(square)] & (@intCast(Bitboard, 1) << square2) != 0) {
            //         }
            //     }
            // }
        }
    }

    pub fn attacksBB(self: @This(), comptime pieceType: PieceType, square: Square, occupied: Bitboard) Bitboard {
        const sq = @enumToInt(square);
        return switch (pieceType) {
            PieceType.Bishop => self.bishopMagics[sq].attacks[self.bishopMagics[sq].index(occupied)],
            PieceType.Rook => self.rookMagics[sq].attacks[self.rookMagics[sq].index(occupied)],
            PieceType.Queen => self.attacksBB(PieceType.Bishop, square, occupied) | self.attacksBB(PieceType.Rook, square, occupied),
            else => self.pseudoAttacks[@enumToInt(pieceType)][sq],
        };
    }

    fn create() @This() {
        return .{
            .rookMagics = undefined,
            .bishopMagics = undefined,
            .pawnAttacks = undefined,
            .pseudoAttacks = undefined,
        };
    }

    fn safeSquare(square: Square, step: i8) Bitboard {
        if (@as(i8, @enumToInt(square)) + step < 0 or @as(i8, @enumToInt(square)) + step > 63) {
            return 0;
        }

        const newSquare: Square = @intToEnum(Square, @intCast(u6, @enumToInt(square) + step));

        const rDiff = std.math.absInt(@as(i6, square.rank()) - @as(i6, newSquare.rank())) catch unreachable;
        const fDiff = std.math.absInt(@as(i6, square.file()) - @as(i6, newSquare.file())) catch unreachable;
        return if (rDiff <= 2 and fDiff <= 2) newSquare.toBitboard() else 0;
    }

    fn pawnAttacksBitboard(comptime color: Color, bb: Bitboard) Bitboard {
        if (color == Color.White) {
            return (shift(bb, 7) & NoHFile) | (shift(bb, 9) & NoAFile);
        } else {
            return (shift(bb, -9) & NoHFile) | (shift(bb, -7) & NoAFile);
        }
    }

    fn initMagics(pieceType: PieceType, table: []Bitboard, magics: []Magic) void {
        const seeds: [2][types.N_RANK]u64 = [2][types.N_RANK]u64{ [_]u64{ 8977, 44560, 54343, 38998, 5731, 92505, 104912, 17020 }, [_]u64{ 728, 10316, 55013, 32803, 12281, 15100, 16645, 255 } };

        var occupancy: [4096]Bitboard = undefined;
        var reference: [4096]Bitboard = undefined;
        var epoch: [4096]i32 = [_]i32{0} ** 4096;
        var cnt: i32 = 0;
        var size: usize = 0;

        for (std.enums.values(Square)) |square| {
            const edges: Bitboard = ((Rank1BB | Rank8BB) & ~(Rank1BB << (8 * @as(u6, square.rank())))) | ((FileABB | FileHBB) & ~(FileABB << square.file()));

            var magic: *Magic = &magics[@enumToInt(square)];
            magic.mask = slidingAttack(pieceType, square, 0) & ~edges;
            // magic.shift = (if (Is64Bit) 64 else 32) - @popCount(Bitboard, magic.mask);
            magic.shift = 64 - @popCount(Bitboard, magic.mask);

            magic.attacks = blk: {
                if (square == Square.a1) {
                    break :blk table;
                } else {
                    var a = magics[@enumToInt(square) - 1].attacks[size..];
                    break :blk a;
                }
            };

            var b: Bitboard = 0;
            size = 0;
            while (true) {
                occupancy[size] = b;
                reference[size] = slidingAttack(pieceType, square, b);

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

    fn slidingAttack(pieceType: PieceType, square: Square, occupied: Bitboard) Bitboard {
        const directions: [4]i8 = if (pieceType == PieceType.Rook) [_]i8{ 8, -8, 1, -1 } else [_]i8{ 9, -7, 7, -9 };

        var attacks: Bitboard = 0;
        for (directions) |dir| {
            var s = square;
            while (safeSquare(s, dir) != 0 and (occupied & (@intCast(Bitboard, 1) << @enumToInt(s))) == 0) {
                const ss = @enumToInt(s) + dir;
                attacks |= @intCast(Bitboard, 1) << @intCast(u6, ss);
                s = @intToEnum(Square, ss);
            }
        }

        return attacks;
    }
};

pub fn attacksBitboard(comptime pieceType: PieceType, square: Square, occupied: Bitboard) Bitboard {
    comptime {
        assert(pieceType != PieceType.Pawn);
    }

    _ = occupied;
    return switch (pieceType) {
        PieceType.Bishop => 1,
        else => PreCalcBitboards.pseudoAttacks[@enumToInt(pieceType)][@enumToInt(square)],
    };
}

pub inline fn isSet(bb: Bitboard, square: Square) bool {
    return (bb & @as(Bitboard, 1) << @enumToInt(square) != 0);
}

pub inline fn set(bb: *Bitboard, square: Square) void {
    bb.* = (bb.* | @as(Bitboard, 1) << @enumToInt(square));
}

pub inline fn unset(bb: *Bitboard, square: Square) void {
    bb.* = (bb.* & ~(@as(Bitboard, 1) << @enumToInt(square)));
}

pub inline fn shift(bb: Bitboard, comptime n: i8) Bitboard {
    comptime if (n < 0) {
        return bb >> (n * -1);
    } else {
        return bb << n;
    };
}

pub fn printBB(bb: Bitboard) void {
    std.debug.print("\n", .{});
    var rank: usize = 7;
    while (rank < types.N_RANK) : (rank -= 1) {
        var file: usize = 0;
        while (file < types.N_FILE) : (file += 1) {
            if (file == 0) {
                std.debug.print("{d} | ", .{rank + 1});
            }

            const square = @intCast(types.SquareType, rank * types.N_FILE + file);
            const bit: usize = if (bb & (@as(Bitboard, 1) << square) != 0) 1 else 0;
            std.debug.print("{d} ", .{bit});
        }
        std.debug.print("\n", .{});
        if (rank == 0) {
            break;
        }
    }
    std.debug.print("    ---------------\n", .{});
    std.debug.print("    a b c d e f g h\n\n", .{});
}
