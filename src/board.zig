const std = @import("std");
const types = @import("types.zig");
const bb = @import("bitboard.zig");
const Bitboard = bb.Bitboard;
const hash_keys = @import("hashkey.zig").hash_keys;
const Key = @import("hashkey.zig").Key;

pub const Board = struct {
    board: [types.n_squares]types.Piece,
    by_type_bb: [types.n_piece_types]Bitboard,
    by_color_bb: [types.n_colors]Bitboard,
    castling_rights: types.CastlingRights,
    key: Key,
    ply: u16,
    history_ply: u16,
    en_passant: ?types.Square,
    side: types.Color,

    pub fn emptyBoard() @This() {
        return .{
            .board = [_]types.Piece{types.Piece.no_piece} ** types.n_squares,
            .by_type_bb = [_]Bitboard{@as(Bitboard, 0)} ** types.n_piece_types,
            .by_color_bb = [_]Bitboard{@as(Bitboard, 0)} ** types.n_colors,
            .castling_rights = @bitCast(types.CastlingRights, @as(u4, 0xF)),
            .key = 0,
            .ply = 0,
            .history_ply = 0,
            .en_passant = null,
            .side = types.Color.white,
        };
    }

    pub fn resetBoard(self: *@This()) void {
        self.* = @This().emptyBoard();
    }

    pub fn generateBoardKey(self: @This()) Key {
        var key: Key = 0;

        for (std.enums.values(types.Square)) |square| {
            const piece = self.board[@enumToInt(square)];
            if (piece != types.Piece.no_piece) {
                key ^= hash_keys.piece_square[@enumToInt(piece)][@enumToInt(square)];
            }
        }

        if (self.side == types.Color.white) {
            key ^= hash_keys.side;
        }

        if (self.en_passant) |enPas| {
            key ^= hash_keys.en_passant[enPas.file()];
        }

        key ^= hash_keys.castling[@bitCast(types.CastlingType, self.castling_rights)];

        return key;
    }

    pub fn parseFEN(self: *@This(), fen: []const u8) void {
        self.resetBoard();

        var rank: u8 = 7;
        var file: u8 = 0;
        var index: usize = 0;

        while (index < fen.len) : ({
            index += 1;
            if (fen[index - 1] != '/') file += 1;
        }) {
            const c = fen[index];
            const square = if (rank * types.n_file + file > 63) types.Square.a1 else @intToEnum(types.Square, rank * types.n_file + file);
            switch (c) {
                '/' => {
                    rank -= 1;
                    file = 0;
                },
                'P' => {
                    self.board[@enumToInt(square)] = types.Piece.white_pawn;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.pawn)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.white)], square);
                },
                'N' => {
                    self.board[@enumToInt(square)] = types.Piece.white_knight;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.knight)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.white)], square);
                },
                'B' => {
                    self.board[@enumToInt(square)] = types.Piece.white_bishop;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.bishop)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.white)], square);
                },
                'R' => {
                    self.board[@enumToInt(square)] = types.Piece.white_rook;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.rook)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.white)], square);
                },
                'Q' => {
                    self.board[@enumToInt(square)] = types.Piece.white_queen;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.queen)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.white)], square);
                },
                'K' => {
                    self.board[@enumToInt(square)] = types.Piece.white_king;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.king)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.white)], square);
                },
                'p' => {
                    self.board[@enumToInt(square)] = types.Piece.black_pawn;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.pawn)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.black)], square);
                },
                'n' => {
                    self.board[@enumToInt(square)] = types.Piece.black_knight;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.knight)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.black)], square);
                },
                'b' => {
                    self.board[@enumToInt(square)] = types.Piece.black_bishop;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.bishop)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.black)], square);
                },
                'r' => {
                    self.board[@enumToInt(square)] = types.Piece.black_rook;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.rook)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.black)], square);
                },
                'q' => {
                    self.board[@enumToInt(square)] = types.Piece.black_queen;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.queen)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.black)], square);
                },
                'k' => {
                    self.board[@enumToInt(square)] = types.Piece.black_king;
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.all_pieces)], square);
                    bb.set(&self.by_type_bb[@enumToInt(types.PieceType.king)], square);
                    bb.set(&self.by_color_bb[@enumToInt(types.Color.black)], square);
                },
                '0'...'9' => {
                    file += c - '0' - 1;
                },
                ' ' => break,
                else => unreachable,
            }
        }

        var parts = std.mem.tokenize(u8, fen[index..], " ");

        const side = parts.next().?[0];
        if (side == 'w') {
            self.side = types.Color.white;
        } else {
            self.side = types.Color.black;
        }

        const castling = parts.next().?;
        for (castling) |ch| {
            switch (ch) {
                'K' => self.castling_rights.white_king_side_castle = true,
                'Q' => self.castling_rights.white_queen_side_castle = true,
                'k' => self.castling_rights.black_king_side_castle = true,
                'q' => self.castling_rights.black_queen_side_castle = true,
                '-' => break,
                else => unreachable,
            }
        }

        const enpassant = parts.next().?;
        if (enpassant[0] != '-') {
            const f = enpassant[0] - 'a';
            const r = enpassant[1] - '1';
            self.en_passant = @intToEnum(types.Square, r * types.n_file + f);
        }

        self.key = self.generateBoardKey();
    }

    pub fn print(self: @This()) void {
        std.debug.print("\n", .{});

        var rank: usize = 7;
        while (rank < types.n_rank) : (rank -= 1) {
            var file: usize = 0;
            while (file < types.n_file) : (file += 1) {
                if (file == 0) {
                    std.debug.print("{d} | ", .{rank + 1});
                }

                const square = @intCast(types.SquareType, rank * types.n_file + file);
                const piece: u8 = switch (self.board[square]) {
                    types.Piece.white_pawn => 'P',
                    types.Piece.white_knight => 'N',
                    types.Piece.white_bishop => 'B',
                    types.Piece.white_rook => 'R',
                    types.Piece.white_queen => 'Q',
                    types.Piece.white_king => 'K',
                    types.Piece.black_pawn => 'p',
                    types.Piece.black_knight => 'n',
                    types.Piece.black_bishop => 'b',
                    types.Piece.black_rook => 'r',
                    types.Piece.black_queen => 'q',
                    types.Piece.black_king => 'k',
                    types.Piece.no_piece => '.',
                };
                std.debug.print("{c} ", .{piece});
            }
            std.debug.print("\n", .{});
            if (rank == 0) {
                break;
            }
        }
        std.debug.print("    ---------------\n", .{});
        std.debug.print("    a b c d e f g h\n\n", .{});
        std.debug.print("Side: {}\n", .{self.side});
        std.debug.print("EnPassant: {}\n", .{self.en_passant});
        std.debug.print("Castling: {c}{c}{c}{c}\n", .{
            if (self.castling_rights.white_king_side_castle) @as(u8, 'K') else @as(u8, '-'),
            if (self.castling_rights.white_queen_side_castle) @as(u8, 'Q') else @as(u8, '-'),
            if (self.castling_rights.black_king_side_castle) @as(u8, 'k') else @as(u8, '-'),
            if (self.castling_rights.black_queen_side_castle) @as(u8, 'q') else @as(u8, '-'),
        });
        std.debug.print("Key: 0x{X}\n", .{self.key});
        std.debug.print("\n", .{});
    }
};
