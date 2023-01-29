const std = @import("std");
const assert = std.debug.assert;

const types = @import("types.zig");
const bb = @import("bitboard.zig");
const Bitboard = bb.Bitboard;
const Position = @import("position.zig").Position;
const Move = @import("move.zig").Move;
const movegen = @import("movegen.zig");
const ExtMove = movegen.ExtMove;
const MoveGenType = movegen.MoveGenType;
const Square = types.Square;
const Value = types.Value;
const Depth = types.Depth;

const Stage = enum {
    main_tt,
    capture_init,
    good_capture,
    refutation,
    quiet_init,
    quiet,
    bad_capture,

    evasion_tt,
    evastion_init,
    evasion,

    probcut_tt,
    probcut_init,
    probcut,

    qsearch_tt,
    qcapture_init,
    qcapture,
    qcheck_init,
    qcheck,
};

// fn partialInsertionSort(moves: []ExtMove, limit: i32) void {
    // var sorted_end: i32 = 0;
    // var i: i32 = 1;
    // while (i < moves.len) : (i += 1) {
    //     if (moves[i].value > limit) {
    //         const tmp = i;
    //         sorted_end += 1;
    //         moves[i] = moves[sorted_end];
    //         i = sorted_end;
    //         var q = sorted_end;
    //         while (q != 0 and (q - 1) < tmp) : (q -= 1) {
    //             moves[q] = moves[q - 1];
    //         }
    //     }
    // }
// }

fn StatsEntry(comptime T: type, comptime d: i32) type {
    return struct {
        entry: T,

        pub fn init(v: T) @This() {
            return .{
                .entry = v,
            };
        }

        pub fn set(self: *@This(), v: T) void {
            self.entry = v;
        }

        pub fn shiftLeft(self: *@This(), bonus: i32) void {
            assert(abs(bonus) <= d);
            // comptime {
            //     assert(d <= std.limits<T>::max());
            // }

            self.entry += bonus - self.entry * abs(bonus) / d;

            assert(abs(self.entry) <= d);
        }

        fn abs(num: i32) i32 {
            if (num < 0) {
                return -num;
            } else {
                return num;
            }
        }
    };
}

fn Stats(comptime T: type, comptime d: i32, comptime size: usize, comptime sizes: []usize) type {
    // const StatsNext = Stats(T, d, size, sizes);
    if (sizes.len == 0) {
        const SE = StatsEntry(T, d);
        return struct {
            items: [size]SE,

            pub fn fill(self: *@This(), v: T) void {
                self.items = [_]SE{SE.init(v)} ** size;
            }
        };
    } else {
        const s = sizes[0];
        const NextStats = Stats(T, d, size, sizes[1..]);
        return struct {
            items: [s]NextStats,

            pub fn fill(self: *@This(), v: T) void {
                var i: usize = 0;
                while (i < s) : (i += 1) {
                    self.items[i].fill(v);
                }
            }
        };
    }
}

const not_used: usize = 0;

pub const ButterflyHistory = Stats(i16, 7183, types.n_colors, types.n_squares * types.n_squares);
pub const CounterMoveHistory = Stats(u16, not_used, types.n_pieces, types.n_squares);
pub const CapturePieceToHistory = Stats(i16, 10692, types.n_pieces, types.n_squares, types.n_piece_types);
pub const PieceToHistory = Stats(i16, 29952, types.n_pieces, types.n_squares);
pub const ContinuationHistory = Stats(PieceToHistory, not_used, types.n_pieces, types.n_squares);

pub const MovePicker = struct {
    const PickType = enum(u2) {
        next = 0,
        best = 1,
    };

    threatened_pieces: Bitboard,
    pos: *Position,
    main_history: *ButterflyHistory,
    capture_history: *CapturePieceToHistory,
    continuation_history: **PieceToHistory,
    tt_move: Move,
    refutations: [3]ExtMove,
    cur: usize,
    end_moves: usize,
    end_bad_captures: usize,
    stage: Stage,
    recapture_square: Square,
    threshold: Value,
    depth: Depth,
    moves: [movegen.max_moves]ExtMove,

    fn score(self: *@This(), comptime move_gen_type: MoveGenType) void {
        comptime {
            assert(move_gen_type == .captures or move_gen_type == .quiets or move_gen_type == .evasions);
        }

        var threatened_by_pawn: Bitboard = 0;
        var threatened_by_minor: Bitboard = 0;
        var threatened_by_rook: Bitboard = 0;
        if (move_gen_type == .quiets) {
            const us = self.pos.side_to_move;

            threatened_by_pawn = self.pos.attacksBy(.pawn, us.flip());
            threatened_by_minor = self.pos.attacksBy(.knight, us.flip()) | self.pos.attacksBy(.bishop, us.flip()) | threatened_by_pawn;
            threatened_by_rook = self.pos.attacksBy(.pawn, us.flip()) | threatened_by_minor;

            self.threatened_pieces = (self.pos.piecesByColorAndType(us, .queen) & threatened_by_rook)
                | (self.pos.piecesByColorAndType(us, .rook) & threatened_by_minor)
                | (self.pos.piecesByColorAndTwoTypes(us, .knight, .bishop) & threatened_by_pawn);
        }

        var i: usize = self.cur;
        while (i < self.end_moves) : (i += 1) {
            const m = self.moves[i];
            if (move_gen_type == .captures) {
                m.value = @intToEnum(Value, 6 * @enumToInt(types.piece_value[@enumToInt(types.Phase.mg)][@enumToInt(self.pos.pieceOn(m.move.to))])
                    + self.capture_history.*[@enumToInt(self.pos.movedPiece(m.move.from))][@enumToInt(m.move.to)][@enumToInt(self.pos.pieceOn(m.move.to).typeOf())]);
            } else if (move_gen_type == .quiets) {
                var extra: i32 = 0;
                if (self.threatened_pieces & bb.squareBB(m.move.from)) {
                    if (self.pos.movedPiece(m.move.from).typeOf() == .queen and (bb.squareBB(m.move.to) & threatened_by_rook) == 0) {
                        extra = 50000;
                    } else if (self.pos.movedPiece(m.move.from).typeOf() == .rook and (bb.squareBB(m.move.to) & threatened_by_minor) == 0) {
                        extra = 25000;
                    } else if ((bb.squareBB(m.move.to) & threatened_by_pawn) == 0) {
                        extra = 15000;
                    } else {
                        extra = 0;
                    }
                } else {
                    extra = 0;
                }
                m.value = @intToEnum(Value, 2 * self.main_history.*[@enumToInt(self.pos.side_to_move)][@bitCast(u16, m.move) & 0xFFF]
                    + 2 * self.continuation_history[0].*[@enumToInt(self.pos.movedPiece(m.move.from))][@enumToInt(m.move.to)]
                    + self.continuation_history[1].*[@enumToInt(self.pos.movedPiece(m.move.from))][@enumToInt(m.move.to)]
                    + self.continuation_history[3].*[@enumToInt(self.pos.movedPiece(m.move.from))][@enumToInt(m.move.to)]
                    + self.continuation_history[5].*[@enumToInt(self.pos.movedPiece(m.move.from))][@enumToInt(m.move.to)]
                    + extra
                    + @boolToInt(self.pos.checkSquares(self.pos.movedPiece(m.move.from).typeOf()) & bb.squareBB(m.move.to)) * 16384);
            } else {
                if (self.pos.capture(m.move)) {
                    m.value = types.piece_value[@enumToInt(types.Phase.mg)][@enumToInt(self.pos.pieceOn(m.move.to))]
                        .sub(@intToEnum(Value, @enumToInt(self.pos.movePiece(m.move.from).typeOf())))
                        .add(@intToEnum(Value, (1 << 28)));
                } else {
                    m.value = @intToEnum(Value, self.main_history.*[@enumToInt(self.pos.side_to_move)][@bitCast(u16, m.move) & 0xFFF]
                        + self.continuation_history[0].*[@enumToInt(self.pos.movedPiece(m.move.from))][@enumToInt(m.move.to)]);
                }
            }
        }
    }

    fn select(self: *@This(), comptime t: PickType, filter: *const fn() bool) Move {
        while (self.cur < self.end_moves) {
            if (t == .best) {
                std.mem.swap(ExtMove, self.moves[self.cur], self.moves[@maximum(self.cur, self.end_moves)]);
            }

            if (self.moves[self.cur] != self.tt_move and filter()) {
                self.cur += 1;
                return self.moves[self.cur - 1];
            }

            self.cur += 1;
        }

        return Move.no_move;
    }

    fn incrementStage(self: *@This()) void {
        self.stage = @intToEnum(Stage, @enumToInt(self.stage) + 1);
    }

    pub fn nextMove(self: *@This(), skip_quiets: bool) Move {
        while (true) {
            switch(self.stage) {
                .main_tt, .evasion_tt, .qsearch_tt, .probcut_tt => {
                    self.incrementStage();
                    return self.tt_move;
                },
                .capture_init, .probcut_init, .qcapture_init => {
                    self.cur = 0;
                    self.end_bad_captures = 0;

                    // TODO movegen stuff
                    self.score(.captures);
                    self.incrementStage();
                },
                .good_capture => {
                    if (self.select(.next, &self.good_capture_lambda)) {
                        return self.moves[self.cur - 1];
                    }
                    // TODO refutations
                    self.incrementStage();
                },
                .refutation => {
                    if (self.select(.next, &self.refutation_lambda)) {
                        return self.moves[self.cur - 1];
                    }
                    self.incrementStage();
                },
                .quiet_init => {
                    if (!skip_quiets) {
                        self.cur = self.end_bad_captures;
                        // TODO movegen stuff

                        self.score(.quiets);
                        // TODO Partial sort
                    }

                    self.incrementStage();
                },
                .quiet => {
                    if (!skip_quiets and self.select(.next, &self.quiet_lambda)) {
                        return self.moves[self.cur - 1];
                    }
                    self.cur = 0;
                    self.end_moves = self.end_bad_captures;

                    self.incrementStage();
                },
                .bad_capture => {
                    return self.select(.next, &self.true_lambda);
                },
                .evastion_init => {
                    self.cur = 0;
                    // TODO movegen stuff

                    self.score(.evasions);
                    self.incrementStage();
                },
                .evasion => {
                    return self.select(.best, &self.true_lambda);
                },
                .probcut => {
                    return self.select(.next, &self.probcut_lambda);
                },
                .qcapture => {
                    if (self.select(.next, &self.qcheck_lambda) != Move.no_move) {
                        return self.moves[self.cur - 1];
                    }

                    if (self.depth != types.depth_qs_check) {
                        return Move.no_move;
                    }

                    self.incrementStage();
                },
                .check_init => {
                    self.cur = 0;
                    // TODO movegen stuff
                    self.incrementStage();
                },
                .qcheck => {
                    return self.select(.next, &self.true_lambda);
                },
            }
        }

        assert(false);
        return Move.no_move;
    }

    fn good_capture_lambda(self: @This()) bool {
        if (self.pos.seeGE(self.moves[self.cur], @intToEnum(Value, -69 * self.moves[self.cur].value / 1024))) {
            return true;
        } else {
            std.mem.swap(ExtMove, &self.moves[self.end_bad_captures], &self.moves[self.cur]);
            return false;
        }
    }

    fn quiet_lambda(self: @This()) bool {
        return self.moves[self.cur] != self.refutations[0].move
            and self.moves[self.cur] != self.refutations[1].move
            and self.moves[self.cur] != self.refutations[2].move;
    }

    fn probcut_lambda(self: @This()) bool {
        return self.pos.seeGE(self.moves[self.cur], self.threshold);
    }

    fn qcapture_lambda(self: @This()) bool {
        return self.depth > types.depth_qs_check or self.moves[self.cur].to == self.recapture_square;
    }

    fn true_lambda(self: @This()) bool {
        _ = self;
        return true;
    }
};
