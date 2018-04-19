module PortableDraughtsNotationTests
open Checkers
open Checkers.GameController
open Checkers.Generic
open Checkers.PublicAPI
open Xunit

[<Fact>]
let ``Create controller from FEN string``() =
    let expectedBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; Piece.whiteKing; None; None; None];
            [None; None; None; None; None; Piece.whiteChecker; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedPlayer = Player.White

    let fenString = "[FEN \"W:WK15,19:BK10\"]"
    let controller = controllerFromFen GameVariant.GameVariant.AmericanCheckers fenString
    
    Assert.Equal(expectedPlayer, controller.CurrentPlayer)
    Assert.Equal(expectedBoard, controller.Board)

[<Fact>]
let ``Create FEN from controller string``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; Piece.whiteKing; None; None; None];
            [None; None; None; None; None; Piece.whiteChecker; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedFENString = "[FEN \"W:WK15,19:BK10\"]"
    Assert.Equal(expectedFENString, (createFen GameVariant.PdnMembers.AmericanCheckers Player.White board))

[<Fact>]
let ``Create controller from FEN string: white move does not crash``() =
    let fenString = "[FEN \"W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13\"]"
    let controller = controllerFromFen GameVariant.GameVariant.AmericanCheckers fenString
    let controller = movePiece {Row = 5; Column = 6} {Row = 4; Column = 7} controller

    let move = controller.Value.MoveHistory.Head
    
    Assert.Equal(None, move.BlackMove)
    Assert.Equal("24-20", move.WhiteMove.Value.DisplayString)

[<Fact>]
let ``Create controller from FEN string: won position``() =
    let expectedBoard =
        array2D [
            [None; Piece.whiteKing; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; Piece.whiteChecker];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; None; None];
        ];
    let expectedPlayer = Player.Black

    let fenString = "[FEN \"B:WK1,K2,K9,13,14,17,20,29,30,31:B\"]"
    let controller = controllerFromFen GameVariant.GameVariant.AmericanCheckers fenString
    
    Assert.Equal(expectedPlayer, controller.CurrentPlayer)
    Assert.Equal(expectedBoard, controller.Board)