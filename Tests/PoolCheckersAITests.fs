module PoolCheckersAITests
open Checkers
open Checkers.Generic
open Checkers.AIs.PoolCheckersAI
open Checkers.PortableDraughtsNotation
open Xunit

[<Fact>]
let ``Calculate moves returns correct number of hops``() =
    let moves = calculateMoves Black Board.defaultBoard
    Assert.Equal(7, moves.Length)

[<Fact>]
let ``Calculate moves returns correct number of king hops``() =
    let board =
        array2D [
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = calculateMoves Black board
    Assert.Equal(7, moves.Length)

[<Fact>]
let ``Calculate moves returns correct number of jumps``() =
    let board =
        array2D [
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = calculateMoves Black board
    Assert.Equal(1, moves.Length)

[<Fact>]
let ``Calculate moves returns correct number of moves #2``() =
    let board =
        array2D [
            [Piece.blackKing; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = calculateMoves Black board
    Assert.Equal(1, moves.Length)

[<Fact>]
let ``Calculate moves returns correct number of jumps including flying jump``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; Piece.blackKing; None; None; None];
            [None; None; None; None; None; Piece.whiteChecker; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = calculateMoves Black board
    Assert.Equal(2, moves.Length)

[<Fact>]
let ``Calculate moves prefers jumps to hops``() =
    let board =
        array2D [
            [None; None; Piece.blackChecker; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = calculateMoves Black board
    Assert.Equal(1, moves.Length)

[<Fact>]
let ``Calculate moves prefers jumps to hops 1``() =
    let board =
        array2D [
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; None; None];
            [None; Piece.blackChecker; None; None; None; None; None; Piece.blackChecker];
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; Piece.blackChecker];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; None; None; Piece.blackChecker; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
        ];

    let moves = calculateMoves White board
    Assert.Equal(1, moves.Length)

[<Fact>]
let ``Calculate moves returns multijump``() =
    let board =
        array2D [
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = getPieceJumps {Row = 0; Column = 0} board
    Assert.Equal(2, moves.Length)

[<Fact>]
let ``Calculate moves returns multijump checker jumps backwards``() =
    let board =
        array2D [
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = getPieceJumps {Row = 0; Column = 0} board
    Assert.Equal(3, moves.[0].Length)

[<Fact>]
let ``Calculate moves returns jump: move ends after promotion``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; Piece.blackChecker; None];
            [None; None; None; Piece.blackChecker; None; None; None; Piece.whiteChecker];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = getPieceJumps {Row = 2; Column = 7} board
    Assert.Equal(2, moves.[0].Length)

[<Fact>]
let ``King does not attempt to jump off board``() =
    let controller = controllerFromFen GameVariant.GameVariant.PoolCheckers "[FEN \"B:WK3,13,21,24,29,30,31,32:B4,12\"]"

    let moves = getPieceJumps {Row = 0; Column = 5} controller.Board
    Assert.Equal(0, moves.Length)

[<Fact>]
let ``King does not attempt to jump onto piece``() =
    let controller = controllerFromFen GameVariant.GameVariant.PoolCheckers "[FEN \"W:WK23:B9,14\"]"

    let moves = getPieceJumps {Row = 5; Column = 4} controller.Board
    Assert.Equal(0, moves.Length)