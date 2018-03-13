module BoardTests
open Checkers.Piece
open Checkers.Board
open Xunit

[<Fact>]
let ``Square returns board piece``() =
    let piece = square {Row = 0; Column = 1} defaultBoard
    Assert.Equal(blackChecker, piece)

[<Fact>]
let ``Square returns None``() =
    let piece = square {Row = 0; Column = 0} defaultBoard
    Assert.True(piece.IsNone)