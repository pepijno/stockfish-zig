const std = @import("std");
const types = @import("types.zig");
const Random = @import("random.zig").Random;

pub const Key = u64;

pub const HashKeys = HashKey.create();

const HashKey = struct {
    pieceSquare: [types.N_PIECES][types.N_SQUARES]Key,
    enPassant: [types.N_FILE]Key,
    castling: [types.N_CASTLING_RIGHTS]Key,
    side: Key,
    noPawns: Key,

    fn create() @This() {
        var rng: Random = comptime Random.create(1070372);
        @setEvalBranchQuota(100000);
        return .{
            .pieceSquare = blk: {
                var psq: [types.N_PIECES][types.N_SQUARES]Key = undefined;
                for (std.enums.values(types.Piece)) |piece| {
                    for (std.enums.values(types.Square)) |square| {
                        psq[@enumToInt(piece)][@enumToInt(square)] = rng.rand();
                    }
                }
                break :blk psq;
            },
            .enPassant = blk: {
                var enpas: [types.N_FILE]Key = undefined;
                for (std.enums.values(types.File)) |file| {
                    enpas[@enumToInt(file)] = rng.rand();
                }
                break :blk enpas;
            },
            .castling = blk: {
                var castling: [types.N_CASTLING_RIGHTS]Key = undefined;
                var i: u8 = 0;
                while (i < 16) : (i += 1) {
                    castling[i] = rng.rand();
                }
                break :blk castling;
            },
            .side = rng.rand(),
            .noPawns = rng.rand(),
        };
    }
};
