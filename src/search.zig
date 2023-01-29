const std = @import("std");

const position = @import("position.zig");
const Position = position.Position;
const StateInfo = position.StateInfo;
const movegen = @import("movegen.zig");
const move = @import("move.zig");
const Move = move.Move;
const types = @import("types.zig");
const Value = types.Value;
const Depth = types.Depth;
const Color = types.Color;
const movepick = @import("movepick.zig");
const PieceToHistory = movepick.PieceToHistory;
const tt = @import("tt.zig");

const Stack = struct {
    pv: *Move,
    continuation_history: *PieceToHistory,
    ply: u32,
    current_move: Move,
    excluded_move: Move,
    killers: [2]Move,
    static_eval: Value,
    stat_score: i32,
    move_count: u32,
    in_check: bool,
    tt_pv: bool,
    tt_hit: bool,
    double_extensions: i32,
    cutoff_cnt: u32,
};

const RootMove = struct {
    score: Value = @intToEnum(Value, -1 * @enumToInt(Value.value_infinite)),
    previous_score: Value = @intToEnum(Value, -1 * @enumToInt(Value.value_infinite)),
    average_score: Value = @intToEnum(Value, -1 * @enumToInt(Value.value_infinite)),
    score_lower_bound: bool = false,
    score_upper_bound: bool = false,
    sel_depth: u32 = 0,
    tb_rank: i32 = 0,
    tb_score: Value,
    pv: std.ArrayList(Move),

    pub fn init(allocator: std.mem.Allocator, m: Move) !@This() {
        var root_move: @This() = .{
            .tb_score = .value_draw,
            .pv = std.ArrayList(Move).init(allocator),
        };
        try root_move.pv.insert(m);
    }

    pub fn extractPonderFromTT(self: @This(), pos: *Position) bool {
        _ = self;
        _ = pos;
    }

    pub fn isEqualToMove(self: @This(), m: Move) bool {
        return self.pv.items[0] == m;
    }

    pub fn isLessThan(self: @This(), rm: RootMove) bool {
        if (@enumToInt(rm.score) != @enumToInt(self.score)) {
            return @enumToInt(rm.score) < @enumToInt(self.score);
        } else {
            return @enumToInt(rm.previous_score) < @enumToInt(self.previous_score);
        }
    }
};
const RootMoves = std.ArrayList(RootMove);

const LimitsType = struct {
    search_moves: std.ArrayList(Move),
    time: [types.n_colors]i64,
    inc: [types.n_colors]i64,
    npm_sec: i64,
    move_time: i64,
    start_time: i64,
    moves_to_go: i32,
    depth: i32,
    mate: i32,
    perft: i32,
    infinite: i32,
    nodes: u64,

    pub fn init() @This() {
        return std.mem.zeroes(@This());
    }

    pub fn useTimeManagement(self: @This()) bool {
        return self.time[@enumToInt(Color.white)] != 0 or self.time[@enumToInt(Color.black)] != 0;
    }
};

pub var Limits = LimitsType.init();

const NodeType = enum {
    non_pv,
    pv,
    root,
};

fn futilityMargin(d: Depth, improving: bool) Value {
    return @intToEnum(Value, 165 * (d - @boolToInt(improving)));
}

var reductions = [_]i32{0} ** movegen.max_moves;

fn reduction(i: bool, d: Depth, mn: i32, delta: Value, root_delta: Value) Depth {
    var r = reductions[@enumToInt(d)] * reductions[mn];
    return (r + 1642 - @enumToInt(delta) * 1024 / @enumToInt(root_delta)) / 1024 + @boolToInt(!i and r > 916);
}

fn futilityMoveCount(improving: bool, depth: Depth) i32 {
    return if (improving) 3 + depth * depth else (3 + depth * depth) / 2;
}

fn statBonus(d: Depth) i32 {
    return @minimum((12 * d + 282) * d - 349, 1594);
}

// TODO valueDraw

const Skill = struct {
    level: f64,
    best: Move = Move.no_move,

    pub fn init(skill_level: i32, uci_elo: i32) @This() {
        const level = if (uci_elo != 0) std.math.clamp(std.math.pow(f64, (@intToFloat(f64, uci_elo) - 1346.6) / 143.1, 1 / 0.806), 0.0, 20.0) else @intToFloat(f64, skill_level);
        return .{
            .level = level,
        };
    }

    pub fn enabled(self: @This()) bool {
        return self.level < 20.0;
    }

    pub fn timeToPick(self: @This(), depth: Depth) bool {
        return depth == 1 + @floatToInt(i32, self.level);
    }

    pub fn pickBest(self: @This(), multi_pv: usize) Move {
        _ = self;
        _ = multi_pv;
    }
};

pub fn perft(comptime root: bool, allocator: std.mem.Allocator, pos: *Position, depth: Depth) u64 {
    var out = std.io.getStdOut().writer();
    var buffer = std.io.bufferedWriter(out);
    var buf_out = buffer.writer();

    var start = std.time.milliTimestamp();

    var cnt: u64 = 0;
    var nodes: u64 = 0;
    const leaf = (depth == 2);

    var st = std.mem.zeroes(StateInfo);
    var ml: movegen.MoveList = .{};
    ml.generate(.legal, pos.*);

    var cur: usize = 0;
    while (cur < ml.current) : (cur += 1) {
        const m: move.Move = ml.moves[cur].move;

        if (root and depth <= 1) {
            cnt = 1;
            nodes += 1;
        } else {
            pos.doMoveWithoutCheck(m, &st);
            if (leaf) {
                var mll: movegen.MoveList = .{};
                mll.generate(.legal, pos.*);
                cnt = mll.current;
            } else {
                cnt = perft(false, allocator, pos, depth - 1);
            }
            nodes += cnt;
            pos.undoMove(m);
        }
        if (root) {
            buf_out.print("{s}: {}\n", .{m.toString(allocator), cnt}) catch unreachable;
            buffer.flush() catch unreachable;
        }
    }

    var end = std.time.milliTimestamp();

    if (root) {
        buf_out.print("Time: {}s\n", .{@intToFloat(f32, end - start) / 1000.0}) catch unreachable;
        buffer.flush() catch unreachable;
    }

    return nodes;
}

pub fn init() void {
    var i: usize = 0;
    while (i < movegen.max_moves) : (i += 1) {
        // TODO threads
        reductions[i] = @floatToInt(i32, (20.26 + @log(1) / 2.0) * @log(i));
    }
}

pub fn clear() void {
    // TODO time management
    tt.tt.clear();
    // TODO threads clear
    // TODO tablebase init
}
