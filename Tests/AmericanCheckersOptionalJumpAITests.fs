module AmericanCheckersOptionalJumpAITests
open Checkers
open Checkers.Generic
open Checkers.AIs.AmericanCheckersAI
open Xunit

[<Fact>]
let ``Calculate moves returns correct number of hops``() =
    let moves = calculateMoves false Black Board.defaultBoard
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

    let moves = calculateMoves false Black board
    Assert.Equal(1, moves.Length)

[<Fact>]
let ``Calculate moves returns jumps and hops``() =
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

    let moves = calculateMoves false Black board
    Assert.Equal(2, moves.Length)