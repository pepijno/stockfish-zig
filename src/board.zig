const std = @import("std");
const types = @import("types.zig");
const bb = @import("bitboard.zig");
const Bitboard = bb.Bitboard;
const HashKeys = @import("hashkey.zig").HashKeys;
const Key = @import("hashkey.zig").Key;

pub const Board = struct {
    board: [types.N_SQUARES]types.Piece,
    byTypeBB: [types.N_PIECE_TYPES]Bitboard,
    byColorBB: [types.N_COLORS]Bitboard,
    castlingRights: types.CastlingRights,
    key: Key,
    ply: u16,
    historyPly: u16,
    enPassant: ?types.Square,
    side: types.Color,

    pub fn emptyBoard() @This() {
        return .{
            .board = [_]types.Piece{types.Piece.NoPiece} ** types.N_SQUARES,
            .byTypeBB = [_]Bitboard{@as(Bitboard, 0)} ** types.N_PIECE_TYPES,
            .byColorBB = [_]Bitboard{@as(Bitboard, 0)} ** types.N_COLORS,
            .castlingRights = @bitCast(types.CastlingRights, @as(u4, 0xF)),
            .key = 0,
            .ply = 0,
            .historyPly = 0,
            .enPassant = null,
            .side = types.Color.White,
        };
    }

    pub fn resetBoard(self: *@This()) void {
        self.* = @This().emptyBoard();
    }

    pub fn generateBoardKey(self: @This()) Key {
        var key: Key = 0;

        for (std.enums.values(types.Square)) |square| {
            const piece = self.board[@enumToInt(square)];
            if (piece != types.Piece.NoPiece) {
                key ^= HashKeys.pieceSquare[@enumToInt(piece)][@enumToInt(square)];
            }
        }

        if (self.side == types.Color.White) {
            key ^= HashKeys.side;
        }

        if (self.enPassant) |enPas| {
            key ^= HashKeys.enPassant[enPas.file()];
        }

        key ^= HashKeys.castling[@bitCast(types.CastlingType, self.castlingRights)];

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
            const square = if (rank * types.N_FILE + file > 63) types.Square.a1 else @intToEnum(types.Square, rank * types.N_FILE + file);
            switch (c) {
                '/' => {
                    rank -= 1;
                    file = 0;
                },
                'P' => {
                    self.board[@enumToInt(square)] = types.Piece.WhitePawn;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Pawn)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.White)], square);
                },
                'N' => {
                    self.board[@enumToInt(square)] = types.Piece.WhiteKnight;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Knight)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.White)], square);
                },
                'B' => {
                    self.board[@enumToInt(square)] = types.Piece.WhiteBishop;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Bishop)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.White)], square);
                },
                'R' => {
                    self.board[@enumToInt(square)] = types.Piece.WhiteRook;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Rook)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.White)], square);
                },
                'Q' => {
                    self.board[@enumToInt(square)] = types.Piece.WhiteQueen;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Queen)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.White)], square);
                },
                'K' => {
                    self.board[@enumToInt(square)] = types.Piece.WhiteKing;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.King)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.White)], square);
                },
                'p' => {
                    self.board[@enumToInt(square)] = types.Piece.BlackPawn;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Pawn)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.Black)], square);
                },
                'n' => {
                    self.board[@enumToInt(square)] = types.Piece.BlackKnight;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Knight)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.Black)], square);
                },
                'b' => {
                    self.board[@enumToInt(square)] = types.Piece.BlackBishop;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Bishop)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.Black)], square);
                },
                'r' => {
                    self.board[@enumToInt(square)] = types.Piece.BlackRook;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Rook)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.Black)], square);
                },
                'q' => {
                    self.board[@enumToInt(square)] = types.Piece.BlackQueen;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.Queen)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.Black)], square);
                },
                'k' => {
                    self.board[@enumToInt(square)] = types.Piece.BlackKing;
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.AllPieces)], square);
                    bb.set(&self.byTypeBB[@enumToInt(types.PieceType.King)], square);
                    bb.set(&self.byColorBB[@enumToInt(types.Color.Black)], square);
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
            self.side = types.Color.White;
        } else {
            self.side = types.Color.Black;
        }

        const castling = parts.next().?;
        for (castling) |ch| {
            switch (ch) {
                'K' => self.castlingRights.WhiteKingSideCastle = true,
                'Q' => self.castlingRights.WhiteQueenSideCastle = true,
                'k' => self.castlingRights.BlackKingSideCastle = true,
                'q' => self.castlingRights.BlackKingSideCastle = true,
                '-' => break,
                else => unreachable,
            }
        }

        const enpassant = parts.next().?;
        if (enpassant[0] != '-') {
            const f = enpassant[0] - 'a';
            const r = enpassant[1] - '1';
            self.enPassant = @intToEnum(types.Square, r * types.N_FILE + f);
        }

        self.key = self.generateBoardKey();
    }

    pub fn print(self: @This()) void {
        std.debug.print("\n", .{});

        var rank: usize = 7;
        while (rank < types.N_RANK) : (rank -= 1) {
            var file: usize = 0;
            while (file < types.N_FILE) : (file += 1) {
                if (file == 0) {
                    std.debug.print("{d} | ", .{rank + 1});
                }

                const square = @intCast(types.SquareType, rank * types.N_FILE + file);
                const piece: u8 = switch (self.board[square]) {
                    types.Piece.WhitePawn => 'P',
                    types.Piece.WhiteKnight => 'N',
                    types.Piece.WhiteBishop => 'B',
                    types.Piece.WhiteRook => 'R',
                    types.Piece.WhiteQueen => 'Q',
                    types.Piece.WhiteKing => 'K',
                    types.Piece.BlackPawn => 'p',
                    types.Piece.BlackKnight => 'n',
                    types.Piece.BlackBishop => 'b',
                    types.Piece.BlackRook => 'r',
                    types.Piece.BlackQueen => 'q',
                    types.Piece.BlackKing => 'k',
                    types.Piece.NoPiece => '.',
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
        std.debug.print("EnPassant: {}\n", .{self.enPassant});
        std.debug.print("Castling: {c}{c}{c}{c}\n", .{
            if (self.castlingRights.WhiteKingSideCastle) @as(u8, 'K') else @as(u8, '-'),
            if (self.castlingRights.WhiteQueenSideCastle) @as(u8, 'Q') else @as(u8, '-'),
            if (self.castlingRights.BlackKingSideCastle) @as(u8, 'k') else @as(u8, '-'),
            if (self.castlingRights.BlackQueenSideCastle) @as(u8, 'q') else @as(u8, '-'),
        });
        std.debug.print("Key: 0x{X}\n", .{self.key});
        std.debug.print("\n", .{});
    }
};
