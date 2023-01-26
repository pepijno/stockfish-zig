const std = @import("std");
const assert = std.debug.assert;

const types = @import("types.zig");
const bb = @import("bitboard.zig");
const psqt = @import("psqt.zig");
const Bitboard = bb.Bitboard;
const hashkey = @import("hashkey.zig");
const Key = hashkey.Key;
const move = @import("move.zig");
const MoveType = @import("move.zig").MoveType;

const piece_to_char = [_]u8{ ' ', 'P', 'N', 'B', 'R', 'Q', 'K', ' ', ' ', 'p', 'n', 'b', 'r', 'q', 'k' };

inline fn h1(h: Key) u32 {
    return @intCast(u32, h & 0x1FFF);
}

inline fn h2(h: Key) u32 {
    return @intCast(u32, (h >> 16) & 0x1FFF);
}

const cuckoo = blk: {
    var keys_table = [_]Key{0} ** 8192;
    var moves_table = [_]move.Move{@bitCast(move.Move, @as(u16, 0))} ** 8192;

    var count: u16 = 0;
    for (types.pieces_table) |piece| {
        var sq1: u16 = @enumToInt(types.Square.a1);
        while (sq1 <= @enumToInt(types.Square.h8)) : (sq1 += 1) {
            var sq2: u16 = sq1 + 1;
            while (sq2 <= @enumToInt(types.Square.h8)) : (sq2 += 1) {
                if ((piece.typeOf() != types.PieceType.pawn) and (bb.attacksBB(piece.typeOf(), @intToEnum(types.Square, sq1), 0) & bb.squareBB(@intToEnum(types.Square, sq2))) != 0) {
                    var m = move.Move.normalMove(@intToEnum(types.Square, sq1), @intToEnum(types.Square, sq2));
                    var key = hashkey.hash_keys.piece_square[@enumToInt(piece)][sq1] ^ hashkey.hash_keys.piece_square[@enumToInt(piece)][sq2] ^ hashkey.hash_keys.side;
                    var i = h1(key);
                    while (true) {
                        std.mem.swap(Key, &keys_table[i], &key);
                        std.mem.swap(move.Move, &moves_table[i], &m);
                        if (@bitCast(u16, m) == 0) {
                            break;
                        }
                        i = if (i == h1(key)) h2(key) else h1(key);
                    }

                    count += 1;
                }
            }
        }
    }

    assert(count == 3668);

    break :blk .{
        .keys = keys_table,
        .moves = moves_table,
    };
};

pub const DirtyPiece = struct {
    dirty_num: u8,
    piece: [3]types.Piece,
    from: [3]types.Square,
    to: [3]types.Square,
};

pub const StateInfo = struct {
    pawn_key: Key,
    material_key: Key,
    non_pawn_material: [types.n_colors]types.Value,
    castling_rights: types.CastlingRights,
    rule_50: i16,
    plies_from_null: i16,
    en_passant: ?types.Square,

    key: Key,
    checkers_bb: Bitboard,
    previous: ?*StateInfo,
    blockers_for_king: [types.n_colors]Bitboard,
    pinners: [types.n_colors]Bitboard,
    check_squares: [types.n_piece_types]Bitboard,
    captured_piece: types.Piece,
    repitition: i8,
    //TODO
    // accumulator
    dirty_piece: DirtyPiece,

    pub fn copyToOtherState(self: @This(), new_state: *@This()) void {
        new_state.pawn_key = self.pawn_key;
        new_state.material_key = self.material_key;
        new_state.non_pawn_material = self.non_pawn_material;
        new_state.castling_rights = self.castling_rights;
        new_state.rule_50 = self.rule_50;
        new_state.plies_from_null = self.plies_from_null;
        new_state.en_passant = self.en_passant;
    }
};

pub const Position = struct {
    board: [types.n_squares]types.Piece,
    by_type_bb: [types.n_piece_types]Bitboard,
    by_color_bb: [types.n_colors]Bitboard,
    piece_count: [types.n_pieces]u8,
    castling_rights_mask: [types.n_squares]u4,
    castling_rook_square: [types.n_castling_rights]types.Square,
    castling_path: [types.n_castling_rights]Bitboard,
    state_info: *StateInfo,
    game_ply: u16,
    side_to_move: types.Color,
    psq: types.Score,
    is_chess_960: bool,

    pub fn emptyBoard(state_info: *StateInfo) @This() {
        return .{
            .board = [_]types.Piece{types.Piece.no_piece} ** types.n_squares,
            .by_type_bb = [_]Bitboard{0} ** types.n_piece_types,
            .by_color_bb = [_]Bitboard{0} ** types.n_colors,
            .piece_count = [_]u8{0} ** types.n_pieces,
            .castling_rights_mask = [_]u4{0} ** types.n_squares,
            .castling_rook_square = [_]types.Square{types.Square.a1} ** types.n_castling_rights,
            .castling_path = [_]Bitboard{0} ** types.n_castling_rights,
            .state_info = state_info,
            .game_ply = 0,
            .side_to_move = types.Color.white,
            .psq = types.Score.zero_score,
            .is_chess_960 = false,
        };
    }

    pub fn zero(self: *@This()) void {
        self.board = [_]types.Piece{types.Piece.no_piece} ** types.n_squares;
        self.by_type_bb = [_]Bitboard{0} ** types.n_piece_types;
        self.by_color_bb = [_]Bitboard{0} ** types.n_colors;
        self.piece_count = [_]u8{0} ** types.n_pieces;
        self.castling_rights_mask = [_]u4{0} ** types.n_squares;
        self.castling_rook_square = [_]types.Square{types.Square.a1} ** types.n_castling_rights;
        self.castling_path = [_]Bitboard{0} ** types.n_castling_rights;
        self.game_ply = 0;
        self.side_to_move = types.Color.white;
        self.psq = types.Score.zero_score;
        self.is_chess_960 = false;
    }

    pub inline fn sideToMove(self: @This()) types.Color {
        return self.side_to_move;
    }

    pub inline fn pieceOn(self: @This(), sq: types.Square) types.Piece {
        return self.board[@enumToInt(sq)];
    }

    pub inline fn empty(self: @This(), sq: types.Square) bool {
        return self.pieceOn(sq) == types.Piece.no_piece;
    }

    pub inline fn movedPiece(self: @This(), m: move.Move) types.Piece {
        return self.pieceOn(m.from);
    }

    pub inline fn pieces(self: @This()) Bitboard {
        return self.piecesByType(types.PieceType.all_pieces);
    }

    pub inline fn piecesByType(self: @This(), piece_type: types.PieceType) Bitboard {
        return self.by_type_bb[@enumToInt(piece_type)];
    }

    pub inline fn piecesByTwoTypes(self: @This(), piece_type1: types.PieceType, piece_type2: types.PieceType) Bitboard {
        return self.piecesByType(piece_type1) | self.piecesByType(piece_type2);
    }

    pub inline fn piecesByColor(self: @This(), color: types.Color) Bitboard {
        return self.by_color_bb[@enumToInt(color)];
    }

    pub inline fn piecesByColorAndType(self: @This(), color: types.Color, piece_type: types.PieceType) Bitboard {
        return self.piecesByType(piece_type) & self.piecesByColor(color);
    }

    pub inline fn piecesByColorAndTwoTypes(self: @This(), color: types.Color, piece_type1: types.PieceType, piece_type2: types.PieceType) Bitboard {
        return self.piecesByColor(color) & self.piecesByTwoTypes(piece_type1, piece_type2);
    }

    pub inline fn countByColor(self: @This(), comptime piece_type: types.PieceType, color: types.Color) u8 {
        return self.piece_count[@enumToInt(types.makePiece(color, piece_type))];
    }

    pub inline fn count(self: @This(), comptime piece_type: types.PieceType) u8 {
        return self.countByColor(piece_type, types.Color.white) + self.countByColor(piece_type, types.Color.black);
    }

    pub inline fn square(self: @This(), comptime piece_type: types.PieceType, color: types.Color) types.Square {
        assert(self.countByColor(piece_type, color) == 1);
        return @intToEnum(types.Square, @ctz(Bitboard, self.piecesByColorAndType(color, piece_type)));
    }

    pub inline fn enPassant(self: @This()) ?types.Square {
        return self.state_info.en_passant;
    }

    pub inline fn isOnSemiopenFile(self: @This(), color: types.Color, sq: types.Square) bool {
        return (self.piecesByColorAndType(color, types.PieceType.pawn) & bb.fileBB(sq)) == 0;
    }

    pub inline fn canCastle(self: @This(), cr: types.CastlingRights) bool {
        return self.state_info.castling_rights.hasCastlingRights(cr);
    }

    pub inline fn castlingRights(self: @This(), color: types.Color) types.CastlingRights {
        if (color == types.Color.white) {
            return @intToEnum(types.CastlingRights, @enumToInt(self.castling_rights) & @enumToInt(types.CastlingRights.white_castling));
        } else {
            return @intToEnum(types.CastlingRights, @enumToInt(self.castling_rights) & @enumToInt(types.CastlingRights.black_castling));
        }
    }

    pub inline fn castlingImpeded(self: @This(), cr: types.CastlingRights) bool {
        assert(cr == types.CastlingRights.white_king_side_castle or cr == types.CastlingRights.white_queen_side_castle or cr == types.CastlingRights.black_king_side_castle or cr == types.CastlingRights.black_king_side_castle);

        return self.pieces() & self.castling_path[@enumToInt(cr)];
    }

    pub inline fn castlingRookSquare(self: @This(), cr: types.CastlingRights) types.Square {
        assert(cr == types.CastlingRights.white_king_side_castle or cr == types.CastlingRights.white_queen_side_castle or cr == types.CastlingRights.black_king_side_castle or cr == types.CastlingRights.black_king_side_castle);

        return self.castling_rook_square[@enumToInt(cr)];
    }

    pub inline fn attackersToBySquare(self: @This(), sq: types.Square) Bitboard {
        return self.attackersTo(sq, self.pieces());
    }

    pub inline fn attackersTo(self: @This(), sq: types.Square, occupied: Bitboard) Bitboard {
        return (bb.pawnAttacksBySquare(types.Color.black, sq) & self.piecesByColorAndType(types.Color.white, types.PieceType.pawn))
             | (bb.pawnAttacksBySquare(types.Color.white, sq) & self.piecesByColorAndType(types.Color.black, types.PieceType.pawn))
             | (bb.pseudoAttacksBB(types.PieceType.knight, sq) & self.piecesByType(types.PieceType.knight))
             | (bb.attacksBB(types.PieceType.rook, sq, occupied) & self.piecesByTwoTypes(types.PieceType.rook, types.PieceType.queen))
             | (bb.attacksBB(types.PieceType.bishop, sq, occupied) & self.piecesByTwoTypes(types.PieceType.bishop, types.PieceType.queen))
             | (bb.pseudoAttacksBB(types.PieceType.king, sq) & self.piecesByType(types.PieceType.king));
    }

    pub inline fn attacksBy(self: @This(), comptime piece_type: types.PieceType, color: types.Color) Bitboard {
        if (piece_type == types.PieceType.pawn) {
            if (color == types.Color.white) {
                return bb.pawnAttacksByBitboard(types.Color.white, self.piecesByColorAndType(types.Color.white, types.PieceType.pawn));
            } else {
                return bb.pawnAttacksByBitboard(types.Color.black, self.piecesByColorAndType(types.Color.black, types.PieceType.pawn));
            }
        } else {
            var threats: Bitboard = 0;
            var attackers = self.piecesByColorAndType(color, piece_type);
            while (attackers != 0) {
                threats |= bb.attacksBB(piece_type, bb.popLsb(attackers), self.pieces());
            }
            return threats;
        }
    }

    pub inline fn checkers(self: @This()) Bitboard {
        return self.state_info.checkers_bb;
    }

    pub inline fn blockersForKing(self: @This(), color: types.Color) Bitboard {
        return self.state_info.blockers_for_king[@enumToInt(color)];
    }

    pub inline fn pinners(self: @This(), color: types.Color) Bitboard {
        return self.state_info.pinners[@enumToInt(color)];
    }

    pub inline fn checkSquares(self: @This(), piece_type: types.PieceType) Bitboard {
        return self.state_info.check_squares[@enumToInt(piece_type)];
    }

    pub inline fn pawnPassed(self: @This(), color: types.Color, sq: types.Square) bool {
        return (self.piecesByColorAndType(color.flip(), types.PieceType.pawn) & bb.passedPawnSpan(color, sq)) == 0;
    }

    pub inline fn pawnsOnSameColorSquares(self: @This(), color: types.Color, sq: types.Square) u8 {
        return @popCount(Bitboard, self.piecesByColorAndType(color, types.PieceType.pawn) & (if ((bb.dark_squares & bb.squareBB(sq)) != 0) bb.dark_squares else ~bb.dark_squares));
    }

    pub inline fn key(self: @This()) Key {
        return self.adjustKey(false, self.state_info.key);
    }

    pub inline fn adjustKey(self: @This(), comptime after_move: bool, k: Key) Key {
        if (self.state_info.rule_50 < 14 - @boolToInt(after_move)) {
            return k;
        } else {
            return k ^ (hashkey.makeKey(@intCast(u64, @as(i16, self.state_info.rule_50) - @as(i16, 14 - @boolToInt(after_move)))) / 8);
        }
    }

    pub inline fn pawnKey(self: @This()) Key {
        return self.state_info.pawn_key;
    }

    pub inline fn materialKey(self: @This()) Key {
        return self.state_info.material_key;
    }

    pub inline fn psqScore(self: @This()) types.Score {
        return self.psq;
    }

    pub inline fn psgEgStm(self: @This()) types.Value {
        return (if (self.side_to_move == types.Color.white) 1 else -1) * types.egValue(self.psq);
    }

    pub inline fn nonPawnMaterialByColor(self: @This(), color: types.Color) types.Value {
        return self.state_info.non_pawn_material[@enumToInt(color)];
    }

    pub inline fn nonPawnMaterial(self: @This()) types.Value {
        return self.nonPawnMaterialByColor(types.Color.white) + self.nonPawnMaterialByColor(types.Color.black);
    }

    pub inline fn rule50Count(self: @This()) i16 {
        return self.state_info.rule_50;
    }

    pub inline fn oppositeBishops(self: @This()) bool {
        return self.countByColor(types.PieceType.bishop, types.Color.white) == 1 and self.countByColor(types.PieceType.bishop, types.Color.black) == 1 and bb.oppositeColors(self.square(types.PieceType.bishop, types.Color.white), self.square(types.PieceType.bishop, types.Color.black));
    }

    pub inline fn capture(self: @This(), m: move.Move) bool {
        return (!self.empty(m.to) and m.move_type != MoveType.castling) or m.move_type == MoveType.en_passant;
    }

    pub inline fn capturedPiece(self: @This()) types.Piece {
        return self.state_info.captured_piece;
    }

    pub inline fn putPiece(self: *@This(), piece: types.Piece, sq: types.Square) void {
        self.board[@enumToInt(sq)] = piece;
        self.by_type_bb[@enumToInt(types.PieceType.all_pieces)] |= bb.squareBB(sq);
        self.by_type_bb[@enumToInt(piece.typeOf())] |= bb.squareBB(sq);
        self.by_color_bb[@enumToInt(piece.colorOf())] |= bb.squareBB(sq);
        self.piece_count[@enumToInt(piece)] += 1;
        self.piece_count[@enumToInt(types.makePiece(piece.colorOf(), types.PieceType.all_pieces))] += 1;
        self.psq = self.psq.add(psqt.psq[@enumToInt(piece)][@enumToInt(sq)]);
    }

    pub inline fn removePiece(self: *@This(), sq: types.Square) void {
        const piece = self.board[@enumToInt(sq)];
        self.by_type_bb[@enumToInt(types.PieceType.all_pieces)] ^= bb.squareBB(sq);
        self.by_type_bb[@enumToInt(piece.typeOf())] ^= bb.squareBB(sq);
        self.by_color_bb[@enumToInt(piece.colorOf())] ^= bb.squareBB(sq);
        self.board[@enumToInt(sq)] = types.Piece.no_piece;
        self.piece_count[@enumToInt(piece)] -= 1;
        self.piece_count[@enumToInt(types.makePiece(piece.colorOf(), types.PieceType.all_pieces))] -= 1;
        self.psq = self.psq.sub(psqt.psq[@enumToInt(piece)][@enumToInt(sq)]);
    }

    pub inline fn movePiece(self: *@This(), from: types.Square, to: types.Square) void {
        const piece = self.board[@enumToInt(from)];
        const fromTo = bb.squareBB(from) | bb.squareBB(to);
        self.by_type_bb[@enumToInt(types.PieceType.all_pieces)] ^= fromTo;
        self.by_type_bb[@enumToInt(piece.typeOf())] ^= fromTo;
        self.by_color_bb[@enumToInt(piece.colorOf())] ^= fromTo;
        self.board[@enumToInt(from)] = types.Piece.no_piece;
        self.board[@enumToInt(to)] = piece;
        self.psq = self.psq.sub(psqt.psq[@enumToInt(piece)][@enumToInt(to)]).sub(psqt.psq[@enumToInt(piece)][@enumToInt(from)]);
    }

    pub inline fn doMoveWithoutCheck(self: *@This(), m: move.Move, new_state_info: *StateInfo) void {
        self.doMove(m, new_state_info, self.givesCheck(m));
    }

    pub fn sliderBlockers(self: @This(), sliders: Bitboard, sq: types.Square, pinner_pieces: *Bitboard) Bitboard {
        var blockers: Bitboard = 0;
        pinner_pieces.* = 0;

        var snipers = ((bb.pseudoAttacksBB(types.PieceType.rook, sq) & self.piecesByTwoTypes(types.PieceType.queen, types.PieceType.rook))
                     | (bb.pseudoAttacksBB(types.PieceType.bishop, sq) & self.piecesByTwoTypes(types.PieceType.queen, types.PieceType.bishop))) & sliders;
        var occupancy = self.pieces() ^ snipers;

        while (snipers != 0) {
            const sniper_sq = bb.popLsb(&snipers);
            const b = bb.betweenBB(sq, sniper_sq) & occupancy;

            if (b != 0 and !bb.moreThanOne(b)) {
                blockers |= b;
                if (b & self.piecesByColor(self.pieceOn(sq).colorOf()) != 0) {
                    pinner_pieces.* |= bb.squareBB(sniper_sq);
                }
            }
        }

        return blockers;
    }

    pub fn legal(self: @This(), m: move.Move) bool {
        const us = self.side_to_move;
        const from = m.from;
        var to = m.to;

        assert(self.movedPiece(m).colorOf() == us);
        assert(self.pieceOn(self.square(types.PieceType.king, us)) == types.makePiece(us, types.PieceType.king));

        if (m.move_type == move.MoveType.en_passant) {
            const king_square = self.square(types.PieceType.king, us);
            const cap_square = to.subDirection(types.pawnPush(us));
            const occupied = (self.pieces() ^ bb.squareBB(from) ^ bb.squareBB(cap_square)) | bb.squareBB(to);

            assert(to == self.enPassant().?);
            assert(self.movedPiece(m) == types.makePiece(us, types.PieceType.pawn));
            assert(self.pieceOn(cap_square) == types.makePiece(us.flip(), types.PieceType.pawn));
            assert(self.pieceOn(to) == types.Piece.no_piece);

            return !(bb.attacksBB(types.PieceType.rook, king_square, occupied) & self.piecesByColorAndTwoTypes(us.flip(), types.PieceType.queen, types.PieceType.rook))
                and !(bb.attacksBB(types.PieceType.bishop, king_square, occupied) & self.piecesByColorAndTwoTypes(us.flip(), types.PieceType.queen, types.PieceType.bishop));
        }

        if (m.move_type == move.MoveType.castling) {
            to = types.relativeSquare(us, if (@enumToInt(to) > @enumToInt(from)) types.Square.g1 else types.Square.c1);
            const step = if (@enumToInt(to) > @enumToInt(from)) types.Direction.west else types.Direction.east;

            var s = to;
            while (s != from) : (s = s.addDirection(step)) {
                if ((self.attackersToBySquare(s) & self.piecesByColor(us.flip())) != 0) {
                    return false;
                }
            }

            return !self.is_chess_960 or !(self.blockersForKing(us) & bb.squareBB(m.to));
        }

        if (self.pieceOn(from).typeOf() == types.PieceType.king) {
            return (self.attackersTo(to, self.pieces() ^ bb.squareBB(from)) & self.piecesByColor(us.flip())) == 0;
        }

        return (self.blockersForKing(us) & bb.squareBB(from)) == 0 or bb.aligned(from, to, self.square(types.PieceType.king, us));
    }

    pub fn pseudoLegal(self: @This(), m: move.Move) bool {
        const us = self.side_to_move;
        const from = m.from;
        const to = m.to;
        const piece = self.movedPiece(m);

        if (m.move_type != move.MoveType.normal) {
            // TODO movelist stuff
            // return if (self.checkers() != 0) 
        }

        if (m.promotion != 0) {
            return false;
        }

        if (piece == types.Piece.no_piece or piece.colorOf() != us) {
            return false;
        }

        if ((self.piecesByColor(us) & bb.squareBB(to)) != 0) {
            return false;
        }

        if (piece.typeOf() == types.PieceType.pawn) {
            if (((bb.rank_8_bb | bb.rank_1_bb) & bb.squareBB(to)) != 0) {
                return false;
            }

            if ((bb.pawnAttacksBySquare(us, from) & self.piecesByColor(us.flip()) & bb.squareBB(to)) == 0
                and !((from.addDirection(types.pawnPush(us)) == to) and self.empty(to))
                and !((from.addDirection(types.pawnPush(us)).addDirection(types.pawnPush(us)) == to)
                    and (types.relativeRankBySquare(us, from) == types.Rank.rank2)
                    and self.empty(to)
                    and self.empty(to.subDirection(types.pawnPush(us))))) {
                return false;
            }
        } else if ((bb.attacksBB(piece.typeOf(), from, self.pieces()) & bb.squareBB(to)) == 0) {
            return false;
        }

        if (self.checkers() != 0) {
            if (piece.typeOf() != types.PieceType.king) {
                if (bb.moreThanOne(self.checkers())) {
                    return false;
                }

                if ((bb.betweenBB(self.square(types.PieceType.king, us), @intToPtr(types.Square, @ctz(Bitboard, self.checkers()))) & bb.squareBB(to)) == 0) {
                    return false;
                }
            } else if ((self.attackersTo(to, self.pieces() ^ bb.squareBB(from)) & self.piecesByColor(us.flip())) != 0) {
                return false;
            }
        }

        return true;
    }

    pub fn givesCheck(self: @This(), m: move.Move) bool {
        assert(self.movedPiece(m).colorOf() == self.side_to_move);

        const from = m.from;
        const to = m.to;

        if ((self.checkSquares(self.pieceOn(from).typeOf()) & bb.squareBB(to)) != 0) {
            return true;
        }

        if ((self.blockersForKing(self.side_to_move.flip()) & bb.squareBB(from)) != 0
            and !bb.aligned(from, to, self.square(types.PieceType.king, self.side_to_move.flip()))) {
            return true;
        }

        switch (m.move_type) {
            move.MoveType.normal => return false,
            move.MoveType.promotion => return (bb.attacksBB(m.promotionToPiece(), to, self.pieces() ^ bb.squareBB(from)) & bb.squareBB(self.square(types.PieceType.king, self.side_to_move.flip()))) != 0,
            move.MoveType.en_passant => {
                const cap_square = types.makeSquare(@intToEnum(types.File, to.file()), @intToEnum(types.Rank, to.rank()));
                const b = (self.pieces() ^ bb.squareBB(from) ^ bb.squareBB(cap_square)) | bb.squareBB(to);

                return ((bb.attacksBB(types.PieceType.rook, self.square(types.PieceType.king, self.side_to_move.flip()), b) & self.piecesByColorAndTwoTypes(self.side_to_move, types.PieceType.queen, types.PieceType.rook))
                    | (bb.attacksBB(types.PieceType.bishop, self.square(types.PieceType.king, self.side_to_move.flip()), b) & self.piecesByColorAndTwoTypes(self.side_to_move, types.PieceType.queen, types.PieceType.bishop))) != 0;
            },
            move.MoveType.castling => {
                const king_square = self.square(types.PieceType.king, self.side_to_move.flip());
                const rto = types.relativeSquare(self.side_to_move, if (@enumToInt(to) > @enumToInt(from)) types.Square.f1 else types.Square.d1);

                return (bb.pseudoAttacksBB(types.PieceType.rook, rto) & bb.squareBB(king_square)) != 0
                    and (bb.attacksBB(types.PieceType.rook, rto, self.pieces() ^ bb.squareBB(from) ^ bb.squareBB(to)) & bb.squareBB(king_square)) != 0;
            },
        }
    }

    pub fn doMove(self: *@This(), m: move.Move, new_state: *StateInfo, gives_check: bool) void {
        var k = self.state_info.key ^ hashkey.hash_keys.side;

        self.state_info.copyToOtherState(new_state);
        new_state.previous = self.state_info;
        self.state_info = new_state;

        self.game_ply += 1;
        self.state_info.rule_50 +=1;
        self.state_info.plies_from_null +=1;

        // TODO nnue stuff

        const us = self.side_to_move;
        const them = self.side_to_move.flip();
        const from = m.from;
        var to = m.to;
        const piece = self.pieceOn(from);
        var captured = if (m.move_type == .en_passant) types.makePiece(them, .pawn) else self.pieceOn(to);

        assert(piece.colorOf() == us);
        assert(captured == .no_piece or captured.colorOf() == (if (m.move_type == .castling) them else us));
        assert(captured.typeOf() != types.PieceType.king);

        if (m.move_type == move.MoveType.castling) {
            assert(piece == types.makePiece(us, types.PieceType.king));
            assert(captured == types.makePiece(us, types.PieceType.rook));

            var rfrom: types.Square = undefined;
            var rto: types.Square = undefined;

            self.doCastling(true, us, from, &to, &rfrom, &rto);

            k ^= hashkey.hash_keys.piece_square[@enumToInt(captured)][@enumToInt(rfrom)] ^ hashkey.hash_keys.piece_square[@enumToInt(captured)][@enumToInt(rto)];
            captured = types.Piece.no_piece;
        }

        if (captured != types.Piece.no_piece) {
            var cap_square = to;

            if (captured.typeOf() == types.PieceType.pawn) {
                if (m.move_type == move.MoveType.en_passant) {
                    cap_square = cap_square.subDirection(types.pawnPush(us));

                    assert(piece == types.makePiece(us, types.PieceType.pawn));
                    assert(to == self.state_info.en_passant.?);
                    assert(types.relativeRank(us, @intToEnum(types.Rank, to.rank())) == types.Rank.rank6);
                    assert(self.pieceOn(to) == types.Piece.no_piece);
                    assert(self.pieceOn(cap_square) == types.makePiece(them, types.PieceType.pawn));
                }

                self.state_info.pawn_key ^= hashkey.hash_keys.piece_square[@enumToInt(captured)][@enumToInt(cap_square)];
            } else {
                self.state_info.non_pawn_material[@enumToInt(them)] = self.state_info.non_pawn_material[@enumToInt(them)].sub(types.piece_value[@enumToInt(types.Phase.mg)][@enumToInt(captured)]);
            }

            // TODO nnue stuff

            self.removePiece(cap_square);

            if (m.move_type == move.MoveType.en_passant) {
                self.board[@enumToInt(cap_square)] = types.Piece.no_piece;
            }

            k ^= hashkey.hash_keys.piece_square[@enumToInt(captured)][@enumToInt(cap_square)];
            self.state_info.material_key ^= hashkey.hash_keys.piece_square[@enumToInt(captured)][self.piece_count[@enumToInt(captured)]];
            // TODO prefetch stuff

            self.state_info.rule_50 = 0;
        }

        k ^= hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(from)] ^ hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(to)];

        if (self.state_info.en_passant) |ep| {
            k ^= hashkey.hash_keys.en_passant[ep.file()];
            self.state_info.en_passant = null;
        }

        if (self.state_info.castling_rights != types.CastlingRights.no_castling and (self.castling_rights_mask[@enumToInt(from)] | self.castling_rights_mask[@enumToInt(to)]) != 0) {
            k ^= hashkey.hash_keys.castling[@enumToInt(self.state_info.castling_rights)];
            self.state_info.castling_rights = @intToEnum(types.CastlingRights, @enumToInt(self.state_info.castling_rights) & ~(self.castling_rights_mask[@enumToInt(from)] | self.castling_rights_mask[@enumToInt(to)]));
            k ^= hashkey.hash_keys.castling[@enumToInt(self.state_info.castling_rights)];
        }

        if (m.move_type != move.MoveType.castling) {
            // TODO nnue stuff
            self.movePiece(from, to);
        }

        if (piece.typeOf() == types.PieceType.pawn) {
            if ((@enumToInt(to) ^ @enumToInt(from)) == 16
                and (bb.pawnAttacksBySquare(us, to.subDirection(types.pawnPush(us))) & self.piecesByColorAndType(them, types.PieceType.pawn)) != 0) {
                self.state_info.en_passant = to.subDirection(types.pawnPush(us));
                k ^= hashkey.hash_keys.en_passant[@enumToInt(self.state_info.en_passant.?)];
            } else if (m.move_type == move.MoveType.promotion) {
                const promotion = types.makePiece(us, m.promotionToPiece());

                assert(types.relativeRank(us, @intToEnum(types.Rank, to.rank())) == types.Rank.rank8);

                self.removePiece(to);
                self.putPiece(promotion, to);

                // TODO nnue stuff

                k ^= hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(to)] ^ hashkey.hash_keys.piece_square[@enumToInt(promotion)][@enumToInt(to)];
                self.state_info.pawn_key ^= hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(to)];
                self.state_info.material_key ^= hashkey.hash_keys.piece_square[@enumToInt(promotion)][self.piece_count[@enumToInt(promotion)] - 1] ^ hashkey.hash_keys.piece_square[@enumToInt(piece)][self.piece_count[@enumToInt(piece)]];

                self.state_info.non_pawn_material[@enumToInt(us)] = self.state_info.non_pawn_material[@enumToInt(us)].add(types.piece_value[@enumToInt(types.Phase.mg)][@enumToInt(promotion)]);
            }

            self.state_info.pawn_key ^= hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(from)] ^ hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(to)];

            self.state_info.rule_50 = 0;
        }

        self.state_info.captured_piece = captured;
        self.state_info.key = k;
        self.state_info.checkers_bb = if (gives_check) self.attackersToBySquare(self.square(.king, them)) & self.piecesByColor(us) else 0;

        self.side_to_move = self.side_to_move.flip();

        self.setCheckInfo(self.state_info);

        self.state_info.repitition = 0;
        const end = @minimum(self.state_info.rule_50, self.state_info.plies_from_null);
        if (end >= 4) {
            var stp = self.state_info.previous.?.previous;
            var i: i8 = 4;
            while (i <= end) : (i += 2) {
                stp = stp.?.previous.?.previous;
                if (stp.?.key == self.state_info.key) {
                    self.state_info.repitition = if (stp.?.repitition != 0) -i else i;
                    break;
                }
            }
        }

        assert(self.posIsOk());
    }

    pub fn undoMove(self: *@This(), m: move.Move) void {
        self.side_to_move = self.side_to_move.flip();

        const us = self.side_to_move;
        const from = m.from;
        const to = m.to;
        var piece = self.pieceOn(to);

        assert(self.empty(from) or m.move_type == .castling);
        assert(self.state_info.captured_piece.typeOf() != .king);

        if (m.move_type == .promotion) {
            assert(types.relativeRankBySquare(us, to) == .rank8);
            assert(piece.typeOf() == m.promotionToPiece());

            self.removePiece(to);
            piece = types.makePiece(us, .pawn);
            self.putPiece(piece, to);
        }

        if (m.move_type == .castling) {
            var rfrom: types.Square = undefined;
            var rto: types.Square = undefined;
            self.doCastling(false, us, from, &to, &rfrom, &rto);
        } else {
            self.movePiece(to, from);

            if (self.state_info.captured_piece != .no_piece) {
                var cap_square: types.Square = to;

                if (m.move_type == .en_passant) {
                    cap_square = cap_square.subDirection(types.pawnPush(us));

                    assert(piece.typeOf() == .pawn);
                    assert(to == self.state_info.en_passant.?);
                    assert(types.relativeRankBySquare(us, to) == .rank6);
                    assert(self.pieceOn(cap_square) == .no_piece);
                    assert(self.state_info.captured_piece == types.makePiece(us.flip(), .pawn()));
                }

                self.putPiece(self.state_info.captured_piece, cap_square);
            }
        }

        self.state_info = self.state_info.previous;
        self.game_ply -= 1;

        assert(self.popIsOk());
    }

    pub fn doCastling(self: *@This(), comptime do: bool, us: types.Color, from: types.Square, to: *types.Square, rfrom: *types.Square, rto: *types.Square) void {
        const king_side = @enumToInt(to.*) > @enumToInt(from);
        rfrom.* = to.*;
        rto.* = types.relativeSquare(us, if (king_side) .f1 else types.Square.d1);
        to.* = types.relativeSquare(us, if (king_side) .g1 else types.Square.c1);

        // TODO nnue stuff
        self.removePiece(if (do) from else to.*);
        self.removePiece(if (do) rfrom.* else rto.*);
        self.board[if (do) @enumToInt(from) else @enumToInt(to.*)] = types.Piece.no_piece;
        self.board[if (do) @enumToInt(rfrom.*) else @enumToInt(rto.*)] = types.Piece.no_piece;
        self.putPiece(types.makePiece(us, types.PieceType.king), if (do) to.* else from);
        self.putPiece(types.makePiece(us, types.PieceType.rook), if (do) rto.* else rfrom.*);
    }

    pub fn doNullMove(self: *@This(), new_state: *StateInfo) void {
        assert(!self.checkers());
        assert(new_state != self.state_info);

        self.state_info.copyToOtherState(new_state);

        new_state.previous = self.state_info;
        self.state_info = new_state;

        self.state_info.dirty_piece.dirty_num = 0;
        self.state_info.dirty_piece.piece[0] = .no_piece;
        // TODO accumulator

        if (self.state_info.en_passant) |ep| {
            self.state_info.key ^= hashkey.hash_keys.en_passant[ep.file()];
            self.state_info.en_passant = null;
        }

        self.state_info.key ^= hashkey.hash_keys.side;
        self.state_info.rule_50 += 1;
        // TODO prefetch transposition table

        self.state_info.plies_from_null = 0;

        self.side_to_move = self.side_to_move.flip();

        self.setCheckInfo(self.state_info);

        self.state_info.repetition = 0;

        assert(self.posIsOk());
    }

    pub fn undoNullMove(self: *@This()) void {
        assert(!self.checkers());
        self.state_info = self.state_info.?.previous;
        self.side_to_move = self.side_to_move.flip();
    }

    pub fn keyAfter(self: @This(), m: move.Move) Key {
        const from = m.from;
        const to = m.to;
        const piece = self.pieceOn(m.from);
        const captured = self.pieceOn(m.to);
        var k = self.state_info.key ^ hashkey.hash_keys.side;

        if (captured != .no_piece) {
            k ^= hashkey.hash_keys.piece_square[@enumToInt(captured)][@enumToInt(to)];
        }

        k ^= hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(to)] ^ hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(from)];

        return if (captured != .no_piece or piece.typeOf() == .pawn) k else self.adjustKey(true, k);
    }

    pub fn seeGE(self: @This(), m: move.Move, threshold: types.Value) bool {
        if (m.move_type != .normal) {
            return @enumToInt(.value_draw) >= @enumToInt(threshold);
        }

        const from = m.from;
        const to = m.to;

        var swap = .piece_value[@enumToInt(.mg)][@enumToInt(self.pieceOn(to))].sub(threshold);
        if (@enumToInt(swap) < 0) {
            return false;
        }

        swap = .piece_value[@enumToInt(.mg)][@enumToInt(self.pieceOn(from))].sub(threshold);
        if (@enumToInt(swap) <= 0) {
            return true;
        }

        assert(self.pieceOn(from).colorOf() == self.side_to_move);
        var occupied = self.pieces() ^ bb.squareBB(from) ^ bb.squareBB(to);
        var stm = self.side_to_move;
        var attackers = self.attackersTo(to, occupied);
        var stm_attackers: Bitboard = undefined;
        var bitboard: Bitboard = undefined;
        var res: i32 = 1;

        while (true) {
            stm = stm.flip();
            attackers &= occupied;

            stm_attackers = attackers & self.piecesByColor(stm);
            if (stm_attackers == 0) {
                break;
            }

            if ((self.pinners(stm.flip()) & occupied) != 0) {
                stm_attackers &= ~self.blockersForKing(stm);

                if (stm_attackers == 0) {
                    break;
                }
            }

            res ^= 1;

            if ((stm_attackers & self.piecesByType(.pawn)) != 0) {
                bitboard = stm_attackers & self.piecesByType(.pawn);
                swap = .pawn_value_mg.sub(swap);
                if (@enumToInt(swap) < res) {
                    break;
                }

                occupied ^= bb.leastSignificantSquareBB(bitboard);
                attackers |= bb.attacksBB(.bishop, to, occupied) & self.piecesByTwoTypes(.bishop, .queen);
            } else if ((stm_attackers & self.piecesByType(.knight)) != 0) {
                bitboard = stm_attackers & self.piecesByType(.knight);
                swap = .knight_value_mg.sub(swap);
                if (@enumToInt(swap) < res) {
                    break;
                }

                occupied ^= bb.leastSignificantSquareBB(bitboard);
            } else if ((stm_attackers & self.piecesByType(.bishop)) != 0) {
                bitboard = stm_attackers & self.piecesByType(.bishop);
                swap = .bishop_value_mg.sub(swap);
                if (@enumToInt(swap) < res) {
                    break;
                }

                occupied ^= bb.leastSignificantSquareBB(bitboard);
                attackers |= bb.attacksBB(.bishop, to, occupied) & self.piecesByTwoTypes(.bishop, .queen);
            } else if ((stm_attackers & self.piecesByType(.rook)) != 0) {
                bitboard = stm_attackers & self.piecesByType(.rook);
                swap = .rook_value_mg.sub(swap);
                if (@enumToInt(swap) < res) {
                    break;
                }

                occupied ^= bb.leastSignificantSquareBB(bitboard);
                attackers |= bb.attacksBB(.rook, to, occupied) & self.piecesByTwoTypes(.rook, .queen);
            } else if ((stm_attackers & self.piecesByType(.queen)) != 0) {
                bitboard = stm_attackers & self.piecesByType(.queen);
                swap = .queen_value_mg.sub(swap);
                if (@enumToInt(swap) < res) {
                    break;
                }

                occupied ^= bb.leastSignificantSquareBB(bitboard);
                attackers |= bb.attacksBB(.bishop, to, occupied) & self.piecesByTwoTypes(.bishop, .queen)
                    | bb.attacksBB(.rook, to, occupied) & self.piecesByTwoTypes(.rook, .queen);
            } else {
                return if ((attackers & ~self.piecesByColor(stm)) != 0) res ^ 1 else res;
            }
        }

        return res != 0;
    }

    pub fn isDraw(self: @This(), ply: i32) bool {
        // TODO movelist stuff
        if (self.state_info.rule_50 > 99 and (!self.checkers())) {
            return true;
        }

        return self.state_info.repitition != 0 and self.state_info.repitition < ply;
    }

    pub fn hasRepeated(self: @This()) bool {
        var stc: *StateInfo = self.state_info;
        var end = @minimum(self.state_info.rule_50, self.state_info.plies_from_null);
        while (end >= 4) : (end -= 1) {
            if (stc.repetition != 0) {
                return true;
            }

            stc = stc.previous.?;
        }

        return false;
    }

    pub fn hasGameCycle(self: @This(), ply: i32) bool {
        const end = @minimum(self.state_info.rule_50, self.state_info.plies_from_null);

        if (end < 3) {
            return false;
        }

        const original_key = self.state_info.key;
        var stp: *StateInfo = self.state_info.previous.?;

        var i = 3;
        while (i <= end) : (i += 2) {
            stp = stp.previous.?.previous.?;

            const move_key = original_key ^ stp.key;
            const j1 = h1(move_key);
            const j2 = h2(move_key);
            if (cuckoo.keys[j1] == move_key or cuckoo.keys[j2] == move_key) {
                const j = if (cuckoo.keys[j1] == move_key) j1 else j2;
                const m = cuckoo.moves[j];
                const sq1 = m.from;
                const sq2 = m.to;

                if (((bb.betweenBB(sq1, sq2) ^ bb.squareBB(sq2)) & self.pieces()) == 0) {
                    if (ply > i) {
                        return true;
                    }

                    if (self.pieceOn(if (self.empty(sq1)) sq2 else sq1).colorOf() != self.side_to_move) {
                        continue;
                    }

                    if (stp.repitition != 0) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    pub fn posIsOk(self: @This()) bool {
        if ((self.pieceOn(self.square(types.PieceType.king, types.Color.white)) != types.Piece.white_king)
            or (self.pieceOn(self.square(types.PieceType.king, types.Color.black)) != types.Piece.black_king)
            or (self.enPassant() != null and types.relativeRankBySquare(self.side_to_move, self.enPassant().?) != types.Rank.rank6)) {
            std.debug.print("posIsOk: Default\n", .{});
            return false;
        }

        // TODO FAST

        if (self.piece_count[@enumToInt(types.Piece.white_king)] != 1
            or self.piece_count[@enumToInt(types.Piece.black_king)] != 1
            or (self.attackersToBySquare(self.square(types.PieceType.king, self.side_to_move.flip())) & self.piecesByColor(self.side_to_move)) != 0) {
            std.debug.print("posIsOk: Kings\n", .{});
            return false;
        }

        if (((self.piecesByType(types.PieceType.pawn) & (bb.rank_1_bb | bb.rank_8_bb)) != 0)
            or self.piece_count[@enumToInt(types.Piece.white_pawn)] > 8
            or self.piece_count[@enumToInt(types.Piece.black_pawn)] > 8) {
            std.debug.print("posIsOk: pawns\n", .{});
            return false;
        }

        if (((self.piecesByColor(types.Color.white) & self.piecesByColor(types.Color.black)) != 0)
            or (self.piecesByColor(types.Color.white) | self.piecesByColor(types.Color.black)) != self.pieces()
            or @popCount(Bitboard, self.piecesByColor(types.Color.white)) > 16
            or @popCount(Bitboard, self.piecesByColor(types.Color.black)) > 16) {
            std.debug.print("posIsOk: bitboards\n", .{});
            return false;
        }

        var p1 = @enumToInt(types.PieceType.pawn);
        while (p1 <= @enumToInt(types.PieceType.king)) : (p1 += 1) {
            var p2 = @enumToInt(types.PieceType.pawn);
            while (p2 <= @enumToInt(types.PieceType.king)) : (p2 += 1) {
                if (p1 != p2 and (self.piecesByType(@intToEnum(types.PieceType, p1)) & self.piecesByType(@intToEnum(types.PieceType, p2))) != 0) {
                    std.debug.print("posIsOk: bitboards\n", .{});
                    return false;
                }
            }
        }

        // TODO NNUE alignement

        for (types.pieces_table) |piece| {
            if (self.piece_count[@enumToInt(piece)] != @popCount(Bitboard, self.piecesByColorAndType(piece.colorOf(), piece.typeOf()))
                or self.piece_count[@enumToInt(piece)] != std.mem.count(types.Piece, self.board[0..], ([_]types.Piece{piece})[0..])) {
                std.debug.print("posIsOk: pieces\n", .{});
                return false;
            }
        }

        for (std.enums.values(types.Color)) |color| {
            const rights = if (color == types.Color.white) [_]types.CastlingRights{types.CastlingRights.white_king_side_castle, types.CastlingRights.white_queen_side_castle} else [_]types.CastlingRights{types.CastlingRights.black_king_side_castle, types.CastlingRights.black_queen_side_castle};
            for (rights) |cr| {
                if (!self.canCastle(cr)) {
                    continue;
                }

                if (self.pieceOn(self.castling_rook_square[@enumToInt(cr)]) != types.makePiece(color, types.PieceType.rook)
                    or self.castling_rights_mask[@enumToInt(self.castling_rook_square[@enumToInt(cr)])] != @enumToInt(cr)
                    or (self.castling_rights_mask[@enumToInt(self.square(types.PieceType.king, color))] & @enumToInt(cr)) != @enumToInt(cr)) {
                    std.debug.print("posIsOk: castling\n", .{});
                    return false;
                }
            }
        }

        return true;
    }

    pub fn print(self: @This(), allocator: std.mem.Allocator) !void {
        std.debug.print("\n +---+---+---+---+---+---+---+---+\n", .{});

        var r: i8 = @enumToInt(types.Rank.rank8);
        while (r >= @enumToInt(types.Rank.rank1)) : (r -= 1) {
            for (std.enums.values(types.File)) |f| {
                std.debug.print(" | {c}", .{piece_to_char[@enumToInt(self.pieceOn(types.makeSquare(f, @intToEnum(types.Rank, r))))]});
            }

            std.debug.print(" | {}\n +---+---+---+---+---+---+---+---+\n", .{1 + r});
        }

        std.debug.print("   a   b   c   d   e   f   g   h\n", .{});
        std.debug.print("\nFen: {s}\nKey: 0x{X}\n", .{ try self.fen(allocator), self.key() });
        std.debug.print("Checkers: ", .{});

        var checks = self.checkers();
        while (checks != 0) {
            std.debug.print("{s} ", .{bb.popLsb(&checks).toString(allocator)});
        }
        //TODO tablebase
        std.debug.print("\n", .{});
    }

    pub fn fen(self: @This(), allocator: std.mem.Allocator) ![]const u8 {
        var str: [8][9]u8 = undefined;

        for (std.enums.values(types.Rank)) |r| {
            var len: u8 = 0;

            var f: i8 = @enumToInt(types.File.a);
            while (f <= @enumToInt(types.File.h)) : (f += 1) {
                var empty_cnt: u8 = 0;
                while (f <= @enumToInt(types.File.h) and self.empty(types.makeSquare(@intToEnum(types.File, f), r))) : (f += 1) {
                    empty_cnt += 1;
                }

                if (empty_cnt != 0) {
                    str[@enumToInt(r)][len] = '0' + empty_cnt;
                    len += 1;
                }

                if (f <= @enumToInt(types.File.h)) {
                    str[@enumToInt(r)][len] = piece_to_char[@enumToInt(self.pieceOn(types.makeSquare(@intToEnum(types.File, f), r)))];
                    len += 1;
                }
            }
        }

        std.mem.reverse([9]u8, str[0..]);

        const side_to_move = if (self.side_to_move == types.Color.white) "w" else "b";

        const white_00 = if (self.canCastle(types.CastlingRights.white_king_side_castle)) "K" else "";
        const white_000 = if (self.canCastle(types.CastlingRights.white_queen_side_castle)) "Q" else "";
        const black_00 = if (self.canCastle(types.CastlingRights.black_king_side_castle)) "k" else "";
        const black_000 = if (self.canCastle(types.CastlingRights.black_queen_side_castle)) "q" else "";
        const castl = try std.fmt.allocPrint(allocator, "{s}{s}{s}{s}", .{white_00, white_000, black_00, black_000});
        const castling = if (castl.len == 0) "-" else castl;

        const ep = if (self.enPassant()) |epSquare| epSquare.toString(allocator) else "-";

        return try std.fmt.allocPrint(allocator, "{s}/{s}/{s}/{s}/{s}/{s}/{s}/{s} {s} {s} {s} {d} {d}", .{str[0], str[1], str[2], str[3], str[4], str[5], str[6], str[7], side_to_move, castling, ep, self.state_info.rule_50, 1 + (self.game_ply - @enumToInt(self.side_to_move)) / 2});
    }

    pub fn set(self: *@This(), fen_str: []const u8, is_chess_960: bool, state_info: *StateInfo) void {
        self.zero();
        state_info.* = std.mem.zeroes(StateInfo);
        self.state_info = state_info;

        var sq: i16 = @enumToInt(types.Square.a8);

        var l: usize = 0;
        while (fen_str[l] != ' ') : (l += 1) {
            const c = fen_str[l];
            switch (c) {
                '0'...'9' => sq += (c - '0'),
                '/' => sq += 2 * @enumToInt(types.Direction.south),
                'P', 'N', 'B', 'R', 'Q', 'K', 'p', 'n', 'b', 'r', 'q', 'k' => {
                    const index = std.mem.indexOfScalar(u8, piece_to_char[0..], c).?;
                    self.putPiece(@intToEnum(types.Piece, index), @intToEnum(types.Square, sq));
                    sq += 1;
                },
                else => {},
            }
        }

        l += 1;
        self.side_to_move = if (fen_str[l] == 'w') types.Color.white else types.Color.black;
        l += 1;

        l += 1;
        while (fen_str[l] != ' ') : (l += 1) {
            var rsq: types.Square = undefined;
            var token = fen_str[l];
            const color = if (token >= 'a' and token <= 'z') types.Color.black else types.Color.white;
            const rook = types.makePiece(color, types.PieceType.rook);

            token = if (token >= 'a' and token <= 'z') token - ' ' else token;

            switch (token) {
                'K' => {
                    rsq = types.relativeSquare(color, types.Square.h1);
                    while (self.pieceOn(rsq) != rook) : (rsq = @intToEnum(types.Square, @enumToInt(rsq) - 1)) {}
                },
                'Q' => {
                    rsq = types.relativeSquare(color, types.Square.a1);
                    while (self.pieceOn(rsq) != rook) : (rsq = @intToEnum(types.Square, @enumToInt(rsq) + 1)) {}
                },
                'A'...'H' => {
                    rsq = types.makeSquare(@intToEnum(types.File, token - 'A'), types.relativeRank(color, types.Rank.rank1));
                },
                else => {
                    continue;
                },
            }

            self.setCastlingRights(color, rsq);
        }
        l += 1;

        var en_passant = false;

        if ((fen_str[l] >= 'a' and fen_str[l] <= 'h') and (fen_str[l + 1] == @as(u8, (if (self.side_to_move == types.Color.white) '6' else '3')))) {
            state_info.en_passant = types.makeSquare(@intToEnum(types.File, fen_str[l] - 'a'), @intToEnum(types.Rank, fen_str[l + 1] - '1'));
            l += 2;

            en_passant = bb.pawnAttacksByBitboard(self.side_to_move.flip(), bb.squareBB(state_info.en_passant.?)) & self.piecesByColorAndType(self.side_to_move, types.PieceType.pawn) != 0
                    and (self.piecesByColorAndType(self.side_to_move.flip(), types.PieceType.pawn) & bb.squareBB(state_info.en_passant.?.addDirection(types.pawnPush(self.side_to_move.flip())))) != 0
                    and (self.pieces() & (bb.squareBB(state_info.en_passant.?) | (bb.squareBB(state_info.en_passant.?.addDirection(types.pawnPush(self.side_to_move)))))) == 0;
        } else {
            l += 1;
        }
        l += 1;

        if (!en_passant) {
            state_info.en_passant = null;
        }

        while (fen_str[l] != ' ') : (l += 1) {
            state_info.rule_50 *= 10;
            state_info.rule_50 += fen_str[l] - '0';
        }
        l += 1;

        while (l < fen_str.len and fen_str[l] != ' ') : (l += 1) {
            self.game_ply *= 10;
            self.game_ply += fen_str[l] - '0';
        }

        self.game_ply = @maximum(2 * (self.game_ply - 1), 0) + @enumToInt(self.side_to_move);

        self.is_chess_960 = is_chess_960;
        self.setState(state_info);

        assert(self.posIsOk());
    }

    fn setCastlingRights(self: *@This(), color: types.Color, rfrom: types.Square) void {
        const kfrom = self.square(types.PieceType.king, color);
        const side = if (@enumToInt(kfrom) < @enumToInt(rfrom)) types.CastlingRights.king_side else types.CastlingRights.queen_side;
        const cr = @intToEnum(types.CastlingRights, @enumToInt(side) & @enumToInt(if (color == types.Color.white) types.CastlingRights.white_castling else types.CastlingRights.black_castling));

        self.state_info.castling_rights = self.state_info.castling_rights.addCastlingRights(cr);
        self.castling_rights_mask[@enumToInt(kfrom)] |= @enumToInt(cr);
        self.castling_rights_mask[@enumToInt(rfrom)] |= @enumToInt(cr);
        self.castling_rook_square[@enumToInt(cr)] = rfrom;

        const kto = types.relativeSquare(color, if (cr.hasCastlingRights(types.CastlingRights.king_side)) types.Square.g1 else types.Square.c1);
        const rto = types.relativeSquare(color, if (cr.hasCastlingRights(types.CastlingRights.king_side)) types.Square.f1 else types.Square.d1);

        self.castling_path[@enumToInt(cr)] = (bb.betweenBB(rfrom, rto) | bb.betweenBB(kfrom, kto)) & ~(bb.squareBB(kfrom) | bb.squareBB(kto));
    }

    fn setState(self: @This(), state_info: *StateInfo) void {
        state_info.key = 0;
        state_info.material_key = 0;
        state_info.pawn_key = hashkey.hash_keys.no_pawns;
        state_info.non_pawn_material[@enumToInt(types.Color.white)] = types.Value.value_draw;
        state_info.non_pawn_material[@enumToInt(types.Color.black)] = types.Value.value_draw;
        state_info.checkers_bb = self.attackersToBySquare(self.square(types.PieceType.king, self.side_to_move)) & self.piecesByColor(self.side_to_move.flip());

        self.setCheckInfo(state_info);

        var b = self.pieces();
        while (b != 0) {
            const sq = bb.popLsb(&b);
            const piece = self.pieceOn(sq);
            state_info.key ^= hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(sq)];

            if (piece.typeOf() == types.PieceType.pawn) {
                state_info.pawn_key ^= hashkey.hash_keys.piece_square[@enumToInt(piece)][@enumToInt(sq)];
            } else if (piece.typeOf() != types.PieceType.king) {
                state_info.non_pawn_material[@enumToInt(piece.colorOf())] = state_info.non_pawn_material[@enumToInt(piece.colorOf())].add(types.piece_value[@enumToInt(types.Phase.mg)][@enumToInt(piece)]);
            }
        }

        if (state_info.en_passant) |ep| {
            state_info.key ^= hashkey.hash_keys.en_passant[ep.file()];
        }

        if (self.side_to_move == types.Color.black) {
            state_info.key ^= hashkey.hash_keys.side;
        }

        state_info.key ^= hashkey.hash_keys.castling[@enumToInt(state_info.castling_rights)];

        for (types.pieces_table) |piece| {
            var cnt: usize = 0;
            while (cnt < self.piece_count[@enumToInt(piece)]) : (cnt += 1) {
                state_info.material_key ^= hashkey.hash_keys.piece_square[@enumToInt(piece)][cnt];
            }
        }
    }

    fn setCheckInfo(self: @This(), state_info: *StateInfo) void {
        state_info.blockers_for_king[@enumToInt(types.Color.white)] = self.sliderBlockers(self.piecesByColor(types.Color.black), self.square(types.PieceType.king, types.Color.white), &state_info.pinners[@enumToInt(types.Color.black)]);
        state_info.blockers_for_king[@enumToInt(types.Color.black)] = self.sliderBlockers(self.piecesByColor(types.Color.white), self.square(types.PieceType.king, types.Color.black), &state_info.pinners[@enumToInt(types.Color.white)]);

        const king_square = self.square(types.PieceType.king, self.side_to_move.flip());

        state_info.check_squares[@enumToInt(types.PieceType.pawn)] = bb.pawnAttacksBySquare(self.side_to_move.flip(), king_square);
        state_info.check_squares[@enumToInt(types.PieceType.knight)] = bb.pseudoAttacksBB(types.PieceType.knight, king_square);
        state_info.check_squares[@enumToInt(types.PieceType.bishop)] = bb.pseudoAttacksBB(types.PieceType.bishop, king_square);
        state_info.check_squares[@enumToInt(types.PieceType.rook)] = bb.pseudoAttacksBB(types.PieceType.rook, king_square);
        state_info.check_squares[@enumToInt(types.PieceType.queen)] = state_info.check_squares[@enumToInt(types.PieceType.bishop)] | state_info.check_squares[@enumToInt(types.PieceType.rook)];
        state_info.check_squares[@enumToInt(types.PieceType.king)] = 0;
    }
};
