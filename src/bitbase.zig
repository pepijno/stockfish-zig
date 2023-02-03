const std = @import("std");
const assert = std.debug.assert;

const bb = @import("bitboard.zig");
const types = @import("types.zig");
const Color = types.Color;
const Square = types.Square;
const Rank = types.Rank;
const File = types.File;

const max_index: u64 = 2 * 24 * 64 * 64;

var kpk_bitbase: std.StaticBitSet(max_index) = std.StaticBitSet(max_index).initEmpty();

fn index(side_to_move: Color, black_king_square: Square, white_king_square: Square, piece_square: Square) u32 {
    return @as(u32, @enumToInt(white_king_square)) | (@as(u32, @enumToInt(black_king_square)) << 6) | (@as(u32, @enumToInt(side_to_move)) << 12) | (@as(u32, piece_square.file()) << 13) | (@as(u32, @enumToInt(Rank.rank7) - piece_square.rank()) << 15);
}

const Result = enum(u8) {
    invalid = 0,
    unknown = 1,
    draw = 2,
    win = 4,
    _,

    pub fn bitwiseOr(self: @This(), v: @This()) @This() {
        return @intToEnum(Result, @enumToInt(self) | @enumToInt(v));
    }
};

const KPKPosition = struct {
    king_square: [types.n_colors]Square,
    piece_square: Square,
    result: Result,
    side_to_move: Color,

    fn init(idx: usize) @This() {
        var kpk_position = std.mem.zeroes(KPKPosition);

        kpk_position.king_square[@enumToInt(Color.white)] = @intToEnum(Square, (idx >> 0) & 0x3F);
        kpk_position.king_square[@enumToInt(Color.black)] = @intToEnum(Square, (idx >> 6) & 0x3F);
        kpk_position.side_to_move = @intToEnum(Color, (idx >> 12) & 0x01);
        kpk_position.piece_square = types.makeSquare(@intToEnum(File, (idx >> 13) & 0x3), @intToEnum(Rank, @enumToInt(Rank.rank7) - ((idx >> 15) & 0x7)));

        const black_king = kpk_position.king_square[@enumToInt(Color.black)];
        const white_king = kpk_position.king_square[@enumToInt(Color.white)];

        if (bb.distance(Square, white_king, black_king) <= 1
            or white_king == kpk_position.piece_square
            or black_king == kpk_position.piece_square
            or (kpk_position.side_to_move == .white and (bb.pawnAttacksBySquare(.white, kpk_position.piece_square) & bb.squareBB(black_king)) != 0)) {
            kpk_position.result = .invalid;
        } else if (kpk_position.side_to_move == .white
            and kpk_position.piece_square.rank() == @enumToInt(Rank.rank7)
            and white_king != kpk_position.piece_square.addDirection(.north)
            and (bb.distance(Square, black_king, kpk_position.piece_square.addDirection(.north)) > 1
                or bb.distance(Square, white_king, kpk_position.piece_square.addDirection(.north)) == 1)) {
            kpk_position.result = .win;
        } else if (kpk_position.side_to_move == .black
            and ((bb.pseudoAttacksBB(.king, black_king) & ~(bb.pseudoAttacksBB(.king, white_king) | bb.pawnAttacksBySquare(.white, kpk_position.piece_square))) == 0
                or (bb.pseudoAttacksBB(.king, black_king) & ~bb.pseudoAttacksBB(.king, white_king) & bb.squareBB(kpk_position.piece_square)) != 0)) {
            kpk_position.result = .draw;
        } else {
            kpk_position.result = .unknown;
        }

        return kpk_position;
    }

    fn classify(self: *@This(), db: [max_index]KPKPosition, print: bool) Result {
        const good: Result = if (self.side_to_move == .white) .win else .draw;
        const bad: Result = if (self.side_to_move == .white) .draw else .win;

        var r: Result = .invalid;
        var b = bb.pseudoAttacksBB(.king, self.king_square[@enumToInt(self.side_to_move)]);

        if (print) {
            bb.print(b) catch unreachable;
        }

        while (b != 0) {
            const idx = if (self.side_to_move == .white)
                index(.black, self.king_square[@enumToInt(Color.black)], bb.popLsb(&b), self.piece_square)
            else
                index(.white, bb.popLsb(&b), self.king_square[@enumToInt(Color.white)], self.piece_square);
            if (print) {
                std.debug.print("{} {}\n", .{@enumToInt(db[idx].result), idx});
            }
            r = r.bitwiseOr(db[idx].result);
            if (print) {
                std.debug.print("{} {}\n", .{@enumToInt(r), idx});
            }
        }

        if (self.side_to_move == .white) {
            if (self.piece_square.rank() < @enumToInt(Rank.rank7)) {
                r = r.bitwiseOr(db[index(.black, self.king_square[@enumToInt(Color.black)], self.king_square[@enumToInt(Color.white)], self.piece_square.addDirection(.north))].result);
            }

            if (@intToEnum(Rank, self.piece_square.rank()) == .rank2
                and self.piece_square.addDirection(.north) != self.king_square[@enumToInt(Color.white)]
                and self.piece_square.addDirection(.north) != self.king_square[@enumToInt(Color.black)]) {
                r = r.bitwiseOr(db[index(.black, self.king_square[@enumToInt(Color.black)], self.king_square[@enumToInt(Color.white)], self.piece_square.addDirection(.north).addDirection(.north))].result);
            }
        }

        if ((@enumToInt(r) & @enumToInt(good)) != 0) {
            self.result = good;
        } else if ((@enumToInt(r) & @enumToInt(Result.unknown)) != 0) {
            self.result = .unknown;
        } else {
            self.result = bad;
        }

        return self.result;
    }
};

pub fn probe(white_king_square: Square, white_pawn_square: Square, black_king_square: Square, side_to_move: Color) bool {
    assert(white_pawn_square.file() <= @enumToInt(File.d));
    return kpk_bitbase.isSet(index(side_to_move, black_king_square, white_king_square, white_pawn_square));
}

pub fn init() void {
    // @setEvalBranchQuota(10_000_000);
    var db: [max_index]KPKPosition = std.mem.zeroes([196608]KPKPosition);

    var idx: usize = 0;
    while (idx < max_index) : (idx += 1) {
        db[idx] = KPKPosition.init(idx);
    }

    var repeat = true;
    while (repeat) {
        repeat = false;
        idx = 0;
        while (idx < max_index) : (idx += 1) {
            const res = db[idx].result == .unknown and db[idx].classify(db, false) != .unknown;
            repeat = repeat or res;
        }
    }

    idx = 0;
    while (idx < max_index) : (idx += 1) {
        if (db[idx].result == .win) {
            kpk_bitbase.set(idx);
        }
    }
}
