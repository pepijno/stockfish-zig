const std = @import("std");
const assert = std.debug.assert;

const hashkey = @import("hashkey.zig");
const Key = hashkey.Key;
const types = @import("types.zig");
const Color = types.Color;
const Score = types.Score;
const Square = types.Square;
const File = types.File;
const Rank = types.Rank;
const Value = types.Value;
const CastlingRights = types.CastlingRights;
const HashTable = types.HashTable;
const bb = @import("bitboard.zig");
const Bitboard = bb.Bitboard;
const position = @import("position.zig");
const Position = position.Position;

const S = types.makeScore;
fn V(val: i32) Value {
    return @intToEnum(Value, val);
}

// Pawn penalties
const backward       = S( 6, 19);
const doubled        = S(11, 51);
const doubled_Early  = S(17,  7);
const isolated       = S( 1, 20);
const weak_lever     = S( 2, 57);
const weak_unopposed = S(15, 18);

// Bonus for blocked pawns at 5th or 6th rank
const blocked_pawn = [2]Score{ S(-19, -8), S(-7, 3) };

const blocked_storm = [types.n_rank]Score{
    S(0, 0), S(0, 0), S(64, 75), S(-3, 14), S(-12, 19), S(-7, 4), S(-10, 5)
};

// Connected pawn bonus
const connected = [types.n_rank]Score{ 0, 3, 7, 7, 15, 54, 86 };

// Strength of pawn shelter for our king by [distance from edge][rank].
// RANK_1 = 0 is used for files where we have no pawn, or pawn is behind our king.
const shelter_strength = [_][types.n_rank]Value{
    [_]Value{ V(-2), V(85), V(95), V(53), V(39), V(23), V(25) },
    [_]Value{ V(-55), V(64), V(32), V(-55), V(-30), V(-11), V(-61) },
    [_]Value{ V(-11), V(75), V(19), V(-6), V(26), V(9), V(-47) },
    [_]Value{ V(-41), V(-11), V(-27), V(-58), V(-42), V(-66), V(-163) }
  };

// Danger of enemy pawns moving toward our king by [distance from edge][rank].
// RANK_1 = 0 is used for files where the enemy has no pawn, or their pawn
// is behind our king. Note that UnblockedStorm[0][1-2] accommodate opponent pawn
// on edge, likely blocked by our king.
const unblocked_storm = [_][types.n_rank]Value{
    [_]Value{ V(94), V(-280), V(-170), V(90), V(59), V(47), V(53) },
    [_]Value{ V(43), V(-17), V(128), V(39), V(26), V(-17), V(15) },
    [_]Value{ V(-9), V(62), V(170), V(34), V(-5), V(-20), V(-11) },
    [_]Value{ V(-27), V(-19), V(106), V(10), V(2), V(-13), V(-24) }
  };

const king_on_file = [2][2]Score{
    [_]Score{ S(-18, 11), S(-6, -3) },
    [_]Score{ S(0, 0), S(5, -4) },
};

fn evaluate(comptime us: Color, pos: Position, e: *Entry) Score {
    const them = us.flip();
    const up = types.pawnPush(us);
    const down = @intToEnum(types.Direction, -1 * @enumToInt(up));

    var neighbours: Bitboard = 0;
    var stoppers: Bitboard = 0;
    var support: Bitboard = 0;
    var phalanx: Bitboard = 0;
    var opposed: Bitboard = 0;
    var lever: Bitboard = 0;
    var lever_push: Bitboard = 0;
    var blocked: Bitboard = 0;
    var backw = false;
    var passed = false;
    var doubld: Bitboard = 0;
    var score = .zero_score;

    var b = pos.piecesByColorAndType(us, .pawn);
    const our_pawns = pos.piecesByColorAndType(us, .pawn);
    const their_pawns = pos.piecesByColorAndType(them, .pawn);

    const double_attack_them = bb.doublePawnAttacks(them, their_pawns);

    e.passed_pawns[@enumToInt(us)] = 0;
    e.king_squares[@enumToInt(us)] = .a1;
    e.pawn_attacks[@enumToInt(us)] = bb.pawnAttacksByBitboard(us, our_pawns);
    e.pawn_attacks_span[@enumToInt(us)] = bb.pawnAttacksByBitboard(us, our_pawns);
    e.blocked_count += @popCount(Bitboard, bb.shift(up, our_pawns) & (their_pawns | double_attack_them));

    while (b != 0) {
        const s = bb.popLsb(&b);

        assert(pos.pieceOn(s) == types.makePiece(us, .pawn));

        const r = types.relativeRank(us, s);

        opposed    = their_pawns & bb.forwardFilesBB(us, s);
        blocked     = their_pawns & s.addDirection(up);
        stoppers   = their_pawns & bb.passedPawnSpan(us, s);
        lever      = their_pawns & bb.pawnAttacksBySquare(us, s);
        lever_push = their_pawns & bb.pawnAttacksBySquare(us, s.addDirection(up));
        doubld    = our_pawns   & s.subDirection(up);
        neighbours = our_pawns   & bb.adjacentFiles(s);
        phalanx    = neighbours  & bb.rankBB(s);
        support    = neighbours  & bb.rankBB(s.subDirection(up));

        if (doubld != 0) {
            if ((our_pawns & bb.shift(down, their_pawns | bb.pawnAttacksByBitboard(them, their_pawns))) == 0) {
                score = score.sub(doubled_Early);
            }
        }

        backw = (neighbours & bb.forwardRanksBB(them, s.addDirection(up))) == 0
            and (lever_push | blocked) != 0;

        if (!backw and blocked == 0) {
            e.pawn_attacks_span[@enumToInt(us)] |= bb.pawnAttackSpan(us, s);
        }

        passed = (stoppers ^ lever) == 0
            or ((stoppers ^ lever_push) == 0 and @popCount(Bitboard, phalanx) >= @popCount(Bitboard, lever_push))
            or ( stoppers == blocked and @enumToInt(r) >= @enumToInt(Rank.rank5) and (bb.shift(up, support) & ~(their_pawns | double_attack_them)) != 0);

        passed &= (bb.forwardFilesBB(us, s) & our_pawns) == 0;

        if (passed) {
            e.passed_pawns[@enumToInt(us)] |= bb.squareBB(s);
        }

        if ((support | phalanx) != 0) {
            const v = connected[@enumToInt(r)] * (2 + @boolToInt(phalanx != 0) - @boolToInt(opposed != 0))
                + 22 * @popCount(Bitboard, support);

            score = score.add(types.makeScore(v, v * (@enumToInt(r) - 2) / 4));
        } else if (neighbours == 0) {
            if (opposed != 0
                and (our_pawns & bb.forwardFilesBB(them, s)) != 0
                and (their_pawns & bb.adjacentFiles(s)) == 0) {
                score = score.sub(doubled);
            } else {
                score = score.sub(isolated.add(if (opposed != 0) weak_unopposed else Score.zero_score));
            }
        } else if (backw != 0) {
            score = score.sub(backward.add(weak_unopposed.mul(opposed == 0).mul((~(bb.file_a_bb | bb.file_h_bb) & bb.squareBB(s)) != 0)));
        }

        if (support == 0) {
            score = score.sub(doubled.mul(doubld == 0).add(weak_lever.mul(bb.moreThanOne(lever))));
        }

        if (blocked != 0 and @enumToInt(r) >= @enumToInt(Rank.rank5)) {
            score = score.add(blocked_pawn[@enumToInt(r) - @enumToInt(Rank.rank5)]);
        }
    }

    return score;
}

pub const Entry = struct {
    key: Key,
    scores: [types.n_colors]Score,
    passed_pawns: [types.n_colors]Bitboard,
    pawn_attacks: [types.n_colors]Bitboard,
    pawn_attacks_span: [types.n_colors]Bitboard,
    king_squares: [types.n_colors]Square,
    king_safety: [types.n_colors]Score,
    castling_rights: [types.n_colors]CastlingRights,
    blocked_count: u32,

    pub fn kingSafety(self: @This(), comptime us: Color, pos: Position) Score {
        if (self.king_squares[@enumToInt(us)] == pos.squareByColor(.king, us) and self.castling_rights[@enumToInt(us)] == pos.castlingRights(us)) {
            return self.king_squares[@enumToInt(us)];
        } else {
            self.king_squares[@enumToInt(us)] = self.doKingSafety(us, pos);
            return self.king_squares[@enumToInt(us)];
        }
    }

    pub fn pawnScore(self: @This(), color: Color) Score {
        return self.scores[@enumToInt(color)];
    }

    pub fn passedPawns(self: @This(), color: Color) Bitboard {
        return self.passed_pawns[@enumToInt(color)];
    }

    pub fn pawnAttacks(self: @This(), color: Color) Bitboard {
        return self.pawn_attacks[@enumToInt(color)];
    }

    pub fn pawnAttacksSpan(self: @This(), color: Color) Bitboard {
        return self.pawn_attacks_span[@enumToInt(color)];
    }

    pub fn passedCount(self: @This()) u32 {
        return @popCount(Bitboard, self.passedPawns(.white) | self.passedPawns(.black));
    }

    fn evaluateShelter(self: @This(), comptime us: Color, pos: Position, king_square: Square) Score {
        const them = us.flip();

        var b = pos.piecesByType(.pawn) & ~bb.forwardRanksBB(them, king_square);
        const our_pawns = b & pos.piecesByColor(us) & ~self.pawnAttacks(them);
        const their_pawns = b & pos.piecesByColor(them);

        var bonus = types.makeScore(5, 5);

        const center = std.math.clamp(king_square.file(), @enumToInt(File.b), @enumToInt(File.g));
        var f: u6 = center - 1;
        while (f <= center + 1) : (f += 1) {
            b = our_pawns & bb.fileBB(@intToEnum(File, f));
            const our_rank = if (b != 0) @enumToInt(types.relativeRank(us, bb.frontMostSquare(them, b))) else 0;

            b = their_pawns & bb.fileBB(@intToEnum(File, f));
            const their_rank = if (b != 0) @enumToInt(types.relativeRank(us, bb.frontMostSquare(them, b))) else 0;

            const d = bb.fileEdgeDistance(f);
            bonus = bonus.add(types.makeScore(shelter_strength[d][our_rank], 0));

            if (our_rank != 0 and (our_rank == their_rank - 1)) {
                bonus = bonus.sub(blocked_storm[their_rank]);
            } else {
                bonus = bonus.sub(types.makeScore(unblocked_storm[d][their_rank], 0));
            }
        }

        bonus = bonus.sub(king_on_file[@boolToInt(pos.isOnSemiOpenFile(us, king_square))][@boolToInt(pos.isOnSemiOpenFile(them, king_square))]);

        return bonus;
    }

    inline fn max(a: Score, b: Score) Score {
        return if (types.mgValue(a) < types.mgValue(b)) b else a;
    }

    fn doKingSafety(self: @This(), comptime us: Color, pos: Position) Score {
        const king_square = pos.square(.king, us);
        self.king_squares[@enumToInt(us)] = king_square;
        self.castling_rights[@enumToInt(us)] = pos.castlingRights(us);

        var shelter = self.evaluateShelter(us, pos, king_square);

        const king_side_castle = if (us == .white) .white_king_side_castle else .black_king_side_castle;
        if (pos.canCastle(king_side_castle)) {
            shelter = max(shelter, self.evaluateShelter(us, pos, types.relativeSquare(us, .g1)));
        }
        const queen_side_castle = if (us == .white) .white_queen_side_castle else .black_queen_side_castle;
        if (pos.canCastle(queen_side_castle)) {
            shelter = max(shelter, self.evaluateShelter(us, pos, types.relativeSquare(us, .c1)));
        }

        const pawns = pos.piecesByColorAndType(us, .pawn);
        var min_pawn_dist = 6;

        if (pawns != 0 and bb.attacksBB(.king, king_square) != 0) {
            min_pawn_dist = 1;
        } else {
            while (pawns != 0) {
                min_pawn_dist = @minimum(min_pawn_dist, bb.distance(king_square, bb.popLsb(&pawns)));
            }
        }

        return shelter.sub(types.makeScore(0, 16 * min_pawn_dist));
    }
};

pub var pawn_table = HashTable(Entry, 131072);

pub fn probe(pos: Position) *Entry {
    const key = pos.pawnKey();
    var e = pawn_table[key];

    if (e.key == key) {
        return e;
    }

    e.key = key;
    e.blocked_count = 0;
    e.scores[@enumToInt(Color.white)] = evaluate(.white, pos, e);
    e.scores[@enumToInt(Color.black)] = evaluate(.black, pos, e);

    return e;
}
