module AmericanCheckersAITests
open Checkers
open Checkers.Generic
open Checkers.AIs.AmericanCheckersAI
open Xunit

[<Fact>]
let ``Calculate moves returns correct number of hops``() =
    let moves = calculateMoves Black Board.defaultBoard
    Assert.Equal(7, moves.Length)

[<Fact>]
let ``Calculate moves returns correct number of jumps``() =
    let board =
        array2D [
            [Piece.blackChecker; None; None; None; None; None; None; None];
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
let ``Calculate moves returns multi double jump``() =
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