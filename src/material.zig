const std = @import("std");
const assert = std.debug.assert;

const hashkey = @import("hashkey.zig");
const Key = hashkey.Key;
const endgame = @import("endgame.zig");
const Endgame = endgame.Endgame;
const Endgames = endgame.Endgames;
const EndgameBase = endgame.EndgameBase;
const types = @import("types.zig");
const Value = types.Value;
const ScaleFactor = types.ScaleFactor;
const Score = types.Score;
const Phase = types.Phase;
const Color = types.Color;
const PieceType = types.PieceType;
const HashTable = types.HashTable;
const position = @import("position.zig");
const Position = position.Position;
const bb = @import("bitboard.zig");

const S = types.makeScore;

const quadratic_ours = [_][types.n_piece_types]Score{
    [_]Score{S(1419, 1455), .zero_score, .zero_score, .zero_score, .zero_score, .zero_score},
    [_]Score{S(101, 28), S(37, 39), .zero_score, .zero_score, .zero_score, .zero_score},
    [_]Score{S(57, 64), S(249, 187), S(-49, -62), .zero_score, .zero_score, .zero_score},
    [_]Score{S(0, 0), S(118, 137), S(10, 27), S(0, 0), .zero_score, .zero_score},
    [_]Score{S(-63, -68), S(-5, 3), S(100, 81), S(132, 118), S(-246, -244), .zero_score},
    [_]Score{S(-210, -211), S(37, 14), S(147, 141), S(161, 105), S(-158, -174), S(-9, -31)},
};

const quadratic_theirs = [_][types.n_piece_types]Score{
    [_]Score{.zero_score, .zero_score, .zero_score, .zero_score, .zero_score, .zero_score},
    [_]Score{S(33, 30), .zero_score, .zero_score, .zero_score, .zero_score, .zero_score},
    [_]Score{S(46, 18), S(106, 84), .zero_score, .zero_score, .zero_score, .zero_score},
    [_]Score{S(75, 35), S(59, 44), S(60, 15), .zero_score, .zero_score, .zero_score},
    [_]Score{S(26, 35), S(6, 22), S(38, 39), S(-12, -2), .zero_score, .zero_score},
    [_]Score{S(97, 93), S(100, 163), S(-58, -91), S(112, 192), S(276, 225), .zero_score},
};

const evaluate_kxk = [_]Endgame(.kxk){ Endgame(.kxk).init(.white), Endgame(.kxk).init(.black) };
const scale_kbpsk = [_]Endgame(.kbpsk){ Endgame(.kbpsk).init(.white), Endgame(.kbpsk).init(.black) };
const scale_kqkrps = [_]Endgame(.kqkrps){ Endgame(.kqkrps).init(.white), Endgame(.kqkrps).init(.black) };
const scale_kpsk = [_]Endgame(.kpsk){ Endgame(.kpsk).init(.white), Endgame(.kpsk).init(.black) };
const scale_kpkp = [_]Endgame(.kpkp){ Endgame(.kpkp).init(.white), Endgame(.kpkp).init(.black) };

fn isKXK(pos: Position, us: Color) bool {
    return bb.moreThanOne(pos.piecesByColor(us.flip())) == 0
        and @enumToInt(pos.nonPawnMaterialByColor(us)) >= @enumToInt(.rook_value_mg);
}

fn isKBPsK(pos: Position, us: Color) bool {
    return pos.countByColor(.pawn, us) >= 1
        and @enumToInt(pos.nonPawnMaterialByColor(us)) == @enumToInt(.bishop_value_mg);
}

fn isKQKRPs(pos: Position, us: Color) bool {
    return pos.countByColor(.pawn, us) == 0
        and @enumToInt(pos.nonPawnMaterialByColor(us)) == @enumToInt(.queen_value_mg)
        and pos.countByColor(.rook, us.flip()) == 1
        and pos.countByColor(.pawn, us.flip()) >= 1;
}

fn imbalance(comptime us: Color, piece_count: [][types.n_piece_types]i32) Score {
    const them = us.flip();

    var bonus = Score.zero_score;

    var pt1 = @enumToInt(PieceType.no_piece_type);
    while (pt1 <= @enumToInt(PieceType.queen)) : (pt1 += 1) {
        if (piece_count[@enumToInt(us)][pt1] == 0) {
            continue;
        }

        var v = quadratic_ours[pt1][pt1].mul(@intToEnum(Score, piece_count[@enumToInt(us)][pt1]));

        var pt2 = @enumToInt(PieceType.no_piece_type);
        while (pt2 < pt1) : (pt2 += 1) {
            v = v.add(quadratic_ours[pt1][pt2].mul(@intToEnum(Score, piece_count[@enumToInt(us)][pt2])))
                .add(quadratic_theirs[pt1][pt2].mul(@intToEnum(Score, piece_count[@enumToInt(them)][pt2])));
        }

        bonus = bonus.add(@intToEnum(Score, piece_count[@enumToInt(us)][pt1]).mul(v));
    }

    return bonus;
}

pub const Entry = struct {
    key: Key,
    evaluation_function: ?*EndgameBase(Value),
    scaling_function: [types.n_colors]?*EndgameBase(ScaleFactor),
    score: Score,
    game_phase: i16,
    factor: [types.n_colors]u8,
};

pub var material_table = HashTable(Entry, 8192).init();

pub fn probe(pos: Position) *Entry {
    const key = pos.materialKey();
    var e = material_table.get(key);

    if (e.key == key) {
        return e;
    }

    e.* = std.mem.zeroes(Entry);
    e.key = key;
    e.factor[@enumToInt(Color.white)] = @truncate(u8, @enumToInt(ScaleFactor.scale_factor_normal));
    e.factor[@enumToInt(Color.black)] = @truncate(u8, @enumToInt(ScaleFactor.scale_factor_normal));

    const npm_w = @enumToInt(pos.nonPawnMaterialByColor(.white));
    const npm_b = @enumToInt(pos.nonPawnMaterialByColor(.black));
    const npm = std.math.clamp(npm_w + npm_b, @enumToInt(Value.endgame_limit), @enumToInt(Value.midgame_limit));

    e.game_phase = @intToEnum(Phase, ((npm - @enumToInt(Value.endgame_limit)) * Phase.phase_midgame) / (@enumToInt(Value.midgame_limit) - @enumToInt(Value.endgame_limit)));

    e.evaluation_function = &Endgames.probe(Value, key);

    if (e.evaluation_function) {
        return e;
    }

    for (std.enums.values(Color)) |c| {
        if (isKXK(pos. c)) {
            e.evaluation_function = &evaluate_kxk[@enumToInt(c)];
            return e;
        }
    }

    const sf = &Endgames.probe(ScaleFactor, key);

    if (sf) {
        e.scaling_function[@enumToInt(sf.strong_side)] = sf;
        return e;
    }

    for (std.enums.values(Color)) |c| {
        if (isKBPsK(pos. c)) {
            e.scaling_function[@enumToInt(c)] = &scale_kbpsk[@enumToInt(c)];
            return e;
        } else if (isKQKRPs(pos. c)) {
            e.scaling_function[@enumToInt(c)] = &scale_kqkrps[@enumToInt(c)];
            return e;
        }
    }

    if (npm_w + npm_b == @enumToInt(Value.value_draw) and pos.piecesByType(.pawn) != 0) {
        if (pos.countByColor(.pawn, .black) == 0) {
            assert(pos.countByColor(.pawn, .white) >= 2);

            e.scaling_function[@enumToInt(Color.white)] = &scale_kpsk[@enumToInt(Color.white)];
        } else if (pos.countByColor(.pawn, .white) == 0) {
            assert(pos.countByColor(.pawn, .black) >= 2);

            e.scaling_function[@enumToInt(Color.black)] = &scale_kpsk[@enumToInt(Color.black)];
        } else if (pos.countByColor(.pawn, .white) == 1 and pos.countByColor(.pawn, .black) == 1) {
            e.scaling_function[@enumToInt(Color.white)] = &scale_kpkp[@enumToInt(Color.white)];
            e.scaling_function[@enumToInt(Color.black)] = &scale_kpkp[@enumToInt(Color.black)];
        }
    }

    if (pos.countByColor(.pawn, .white) == 0 and npm_w - npm_b <= @enumToInt(Value.bishop_value_mg)) {
        if (npm_w < @enumToInt(Value.rook_value_mg)) {
            e.factor[@enumToInt(Color.white)] = @truncate(u8, @enumToInt(ScaleFactor.scale_factor_draw));
        } else if (npm_b <= @enumToInt(Value.bishop_value_mg)) {
            e.factor[@enumToInt(Color.white)] = @truncate(u8, 4);
        } else {
            e.factor[@enumToInt(Color.white)] = @truncate(u8, 14);
        }
    }

    if (pos.countByColor(.pawn, .black) == 0 and npm_b - npm_w <= @enumToInt(Value.bishop_value_mg)) {
        if (npm_b < @enumToInt(Value.rook_value_mg)) {
            e.factor[@enumToInt(Color.black)] = @truncate(u8, @enumToInt(ScaleFactor.scale_factor_draw));
        } else if (npm_b <= @enumToInt(Value.bishop_value_mg)) {
            e.factor[@enumToInt(Color.black)] = @truncate(u8, 4);
        } else {
            e.factor[@enumToInt(Color.black)] = @truncate(u8, 14);
        }
    }

    const piece_count = [types.n_colors][6]u8{
        [6]u8{ @as(u8, @boolToInt(pos.countByColor(.bishop, .white) > 1)), pos.countByColor(.pawn, .white), pos.countByColor(.knight, .white),
            pos.countByColor(.bishop, .white), pos.countByColor(.rook, .white), @as(u8, @boolToInt(pos.countByColor(.queen, .white))) },
        [6]u8{ @as(u8, @boolToInt(pos.countByColor(.bishop, .black) > 1)), pos.countByColor(.pawn, .black), pos.countByColor(.knight, .black),
            pos.countByColor(.bishop, .black), pos.countByColor(.rook, .black), @as(u8, @boolToInt(pos.countByColor(.queen, .black))) }
    };

    e.score = @intToEnum(Score, (@enumToInt(imbalance(.white, piece_count)) - @enumToInt(imbalance(.black, piece_count))) / 16);

    return e;
}
