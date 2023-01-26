const std = @import("std");
const types = @import("types.zig");
const Random = @import("random.zig").Random;

pub const Key = u64;

pub const hash_keys = HashKey.create();

const HashKey = struct {
    piece_square: [types.n_pieces][types.n_squares]Key,
    en_passant: [types.n_file]Key,
    castling: [types.n_castling_rights]Key,
    side: Key,
    no_pawns: Key,

    pub fn create() @This() {
        var rng: Random = comptime Random.create(1070372);
        @setEvalBranchQuota(100000);
        return .{
            .piece_square = blk: {
                var psq: [types.n_pieces][types.n_squares]Key = undefined;
                for (types.pieces_table) |piece| {
                    for (std.enums.values(types.Square)) |square| {
                        psq[@enumToInt(piece)][@enumToInt(square)] = rng.rand();
                    }
                }
                break :blk psq;
            },
            .en_passant = blk: {
                var enpas: [types.n_file]Key = undefined;
                for (std.enums.values(types.File)) |file| {
                    enpas[@enumToInt(file)] = rng.rand();
                }
                break :blk enpas;
            },
            .castling = blk: {
                var castling: [types.n_castling_rights]Key = undefined;
                var i: u8 = 0;
                while (i < 16) : (i += 1) {
                    castling[i] = rng.rand();
                }
                break :blk castling;
            },
            .side = rng.rand(),
            .no_pawns = rng.rand(),
        };
    }
};

pub fn makeKey(seed: u64) Key {
    return seed * 6364136223846793005 + 1442695040888963407;
}
