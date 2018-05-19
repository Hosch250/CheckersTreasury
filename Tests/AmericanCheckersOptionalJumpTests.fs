module AmericanCheckersOptionalJumpTests
open Checkers
open Checkers.Variants.AmericanCheckers
open Checkers.Generic
open Xunit

[<Fact>]
let ``Starting coord row must exist``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidMove false {Row = -1; Column = 0} {Row = 0; Column = 0})

[<Fact>]
let ``Starting coord column must exist``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidMove false {Row = 0; Column = -1} {Row = 0; Column = 0})

[<Fact>]
let ``Ending coord row must exist``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidMove false {Row = 0; Column = 0} {Row = -1; Column = 0})

[<Fact>]
let ``Ending coord column must exist``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidMove false {Row = 0; Column = 0} {Row = 0; Column = -1})

[<Fact>]
let ``Checker cannot make flying jump``() =
    let board =
        array2D [
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(board |> isValidMove false {Row = 0; Column = 1} {Row = 3; Column = 4})

[<Fact>]
let ``Move hops piece``() =
    let board =
        array2D [
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.whiteKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 0; Column = 1} {Row = 1; Column = 2}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Hopping black to line 7 promotes``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; Piece.blackKing; None; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 6; Column = 0} {Row = 7; Column = 1}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Hopping white to line 0 promotes``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 1; Column = 1} {Row = 0; Column = 0}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Jumping black to line 7 promotes``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 5; Column = 0} {Row = 7; Column = 2}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Jumping white to line 0 promotes``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.blackChecker; None; None; None; None; None; None];
            [None; None; Piece.whiteChecker; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 2; Column = 2} {Row = 0; Column = 0}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Move jump down right jumps piece``() =
    let board =
        array2D [
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 0; Column = 1} {Row = 2; Column = 3}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Move jump down left jumps piece``() =
    let board =
        array2D [
            [None; None; Piece.whiteKing; None; None; None; None; None];
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 0; Column = 2} {Row = 2; Column = 0}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Move jump up right jumps piece``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.blackKing; None; None; None; None; None; None];
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [None; None; Piece.whiteKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 2; Column = 0} {Row = 0; Column = 2}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Move jump up left jumps piece``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; Piece.whiteKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    let newBoard = (board |> movePiece false {Row = 2; Column = 2} {Row = 0; Column = 0}).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Move sequence jumps pieces``() =
    let board =
        array2D [
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; Piece.blackKing; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let expectedBoard =
        array2D [
            [None; None; None; None; Piece.whiteKing; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    let newBoard = (moveSequence false [{Row = 0; Column = 0}; {Row = 2; Column = 2}; {Row = 0; Column = 4}] (Some <| board)).Value
    Assert.Equal(expectedBoard, newBoard)