module PieceTests

open Checkers
open Checkers.Piece
open Checkers.Generic
open Xunit

[<Fact>]
let ``When I create a White piece, the player is White``() =
    let piece = { Player = Player.White; PieceType = PieceType.Checker }
    Assert.Equal(piece.Player, Player.White)

[<Fact>]
let ``When I create a Black piece, the player is Black``() =
    let piece = { Player = Player.Black; PieceType = PieceType.King }
    Assert.Equal(piece.Player, Player.Black)

[<Fact>]
let ``When I create a Checker, the type is Checker``() =
    let piece = { Player = Player.White; PieceType = PieceType.Checker }
    Assert.Equal(piece.PieceType, PieceType.Checker)

[<Fact>]
let ``When I create a King, the type is King``() =
    let piece = { Player = Player.Black; PieceType = PieceType.King }
    Assert.Equal(piece.PieceType, PieceType.King)

[<Fact>]
let ``When I promote a piece, the type changes to King``() =
    let piece = { Player = Player.Black; PieceType = PieceType.Checker }
    Assert.Equal((Promote piece).PieceType, PieceType.King)