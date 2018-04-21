module PoolCheckersTests
open Checkers
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.PoolCheckers
open Checkers.Generic
open Checkers.PortableDraughtsNotation
open Checkers.GameVariant
open Xunit

[<Fact>]
let ``Starting coord row must exist``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidMove {Row = -1; Column = 0} {Row = 0; Column = 0})

[<Fact>]
let ``Starting coord column must exist``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidMove {Row = 0; Column = -1} {Row = 0; Column = 0})

[<Fact>]
let ``Ending coord row must exist``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidMove {Row = 0; Column = 0} {Row = -1; Column = 0})

[<Fact>]
let ``Ending coord column must exist``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidMove {Row = 0; Column = 0} {Row = 0; Column = -1})

[<Fact>]
let ``Move must be diagonal``() =
    Assert.False(moveIsDiagonal {Row = 2; Column = 1} {Row = 3; Column = 1})

[<Fact>]
let ``End coord must not match start coord``() =
    Assert.False(moveIsDiagonal {Row = 2; Column = 1} {Row = 2; Column = 1})
    
[<Fact>]
let ``Up right moves allowed``() =
    Assert.True(moveIsDiagonal {Row = 2; Column = 1} {Row = 3; Column = 2})

[<Fact>]
let ``Up left moves allowed``() =
    Assert.True(moveIsDiagonal {Row = 2; Column = 1} {Row = 3; Column = 0})
    
[<Fact>]
let ``Down right moves allowed``() =
    Assert.True(moveIsDiagonal {Row = 2; Column = 1} {Row = 1; Column = 2})

[<Fact>]
let ``Down left moves``() =
    Assert.True(moveIsDiagonal {Row = 2; Column = 1} {Row = 1; Column = 0})

[<Fact>]
let ``Black checker can move forward to empty square``() =
    let board = Board.defaultBoard
    Assert.True(board |> isValidCheckerHop{Row = 2; Column = 1} {Row = 3; Column = 0})

[<Fact>]
let ``Black checker cannot move backward``() =
    let board =
        array2D [
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None];
            [None; None; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
        ];

    Assert.False(board |> isValidCheckerHop{Row = 3; Column = 0} {Row = 2; Column = 1})

[<Fact>]
let ``White checker can move forward to empty square``() =
    let board = Board.defaultBoard
    Assert.True(board |> isValidCheckerHop{Row = 5; Column = 0} {Row = 4; Column = 1})

[<Fact>]
let ``White checker cannot move backward``() =
    let board =
        array2D [
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None];
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
        ];

    Assert.False(board |> isValidCheckerHop{Row = 4; Column = 1} {Row = 5; Column = 0})

[<Fact>]
let ``Checker cannot move forward to claimed square``() =
    let board = Board.defaultBoard
    Assert.False(board |> isValidCheckerHop{Row = 1; Column = 0} {Row = 2; Column = 1})

[<Fact>]
let ``Checker can jump forward``() =
    let board =
        array2D [
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None];
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [None; None; Piece.whiteChecker; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
        ];

    Assert.True(board |> isValidCheckerJump{Row = 2; Column = 1} {Row = 4; Column = 3})

[<Fact>]
let ``Checker multijump through king rank does not promote``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; Piece.blackChecker; None; Piece.blackChecker; None];
            [None; None; None; None; None; None; None; Piece.whiteChecker];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let newBoard :Board = uncheckedMoveSequence [{Row = 2; Column = 7}; {Row = 0; Column = 5}; {Row = 2; Column = 3}] board
    let actualPiece = (square {Row = 2; Column = 3} newBoard).Value
    
    Assert.Equal(Checker, actualPiece.PieceType)

[<Fact>]
let ``Turn cannot continue after promotion``() =
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

    let newBoard :Board = uncheckedMoveSequence [{Row = 2; Column = 7}; {Row = 0; Column = 5}] board

    Assert.True(playerTurnEnds [{Row = 2; Column = 7}; {Row = 0; Column = 5}] board newBoard)

[<Fact>]
let ``Checker cannot jump empty square``() =
    let board =
        array2D [
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None];
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
        ];

    Assert.False(board |> isValidCheckerJump{Row = 2; Column = 1} {Row = 4; Column = 3})

[<Fact>]
let ``Checker cannot jump friend``() =
    let board =
        array2D [
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None];
            [None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
        ];

    Assert.False(board |> isValidCheckerJump{Row = 1; Column = 0} {Row = 3; Column = 2})

[<Fact>]
let ``Checker cannot make flying jump``() =
    let board =
        array2D [
            [None; Piece.blackChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; Piece.whiteChecker; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(board |> isValidCheckerJump{Row = 0; Column = 1} {Row = 3; Column = 4})

[<Fact>]
let ``Checker can jump backward``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; Piece.whiteChecker; None; None; None];
            [None; None; None; None; None; Piece.blackChecker; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(board |> isValidCheckerJump{Row = 4; Column = 5} {Row = 3; Column = 4})

[<Fact>]
let ``King can move forward to empty square``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(board |> isValidCheckerHop{Row = 4; Column = 5} {Row = 5; Column = 6})

[<Fact>]
let ``King can move backward to empty square``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(board |> isValidCheckerHop{Row = 4; Column = 5} {Row = 3; Column = 4})

[<Fact>]
let ``King cannot move to claimed square``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; Piece.whiteChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(board |> isValidCheckerHop{Row = 4; Column = 5} {Row = 5; Column = 6})

[<Fact>]
let ``King can jump``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; Piece.whiteChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(board |> isValidKingJump{Row = 4; Column = 5} {Row = 6; Column = 7})

[<Fact>]
let ``King can make flying jump``() =
    let board =
        array2D [
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; Piece.whiteChecker; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(board |> isValidKingJump{Row = 0; Column = 1} {Row = 4; Column = 5})

[<Fact>]
let ``King can make flying jump 1``() =
    let board =
        array2D [
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(board |> isValidKingJump{Row = 0; Column = 3} {Row = 3; Column = 6})

[<Fact>]
let ``King cannot make flying jump over another piece``() =
    let board =
        array2D [
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; Piece.whiteChecker; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; Piece.whiteChecker; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(board |> isValidKingJump{Row = 0; Column = 1} {Row = 4; Column = 5})

[<Fact>]
let ``King can make flying hop``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; Piece.whiteChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    Assert.False(board |> isValidCheckerJump {Row = 4; Column = 5} {Row = 6; Column = 3})

[<Fact>]
let ``King can make flying hop to edge``() =
    let controller = controllerFromFen GameVariant.PoolCheckers "[FEN \"B:WK11:BK10\"]"

    Assert.True(isValidMove {Row = 2; Column = 3} {Row = 0; Column = 5} controller.Board)

[<Fact>]
let ``King cannot jump friend``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; Piece.blackChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    Assert.False(board |> isValidCheckerJump {Row = 4; Column = 5} {Row = 6; Column = 7})

[<Fact>]
let ``King has valid jump``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; Piece.whiteChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    Assert.True(hasValidJump {Row = 4; Column = 5} board)

[<Fact>]
let ``King does not have valid jump``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackKing; None; None];
            [None; None; None; None; None; None; Piece.blackChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    Assert.False(hasValidJump {Row = 4; Column = 5} board)

[<Fact>]
let ``Checker has valid jump``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackChecker; None; None];
            [None; None; None; None; None; None; Piece.whiteChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    Assert.True(hasValidJump {Row = 4; Column = 5} board)

[<Fact>]
let ``Checker does not have valid jump``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; Piece.whiteChecker; None];
            [None; None; None; None; None; None; Piece.blackChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    Assert.False(hasValidJump {Row = 4; Column = 6} board)

[<Fact>]
let ``Other checker has valid jump``() =
    let board =
        array2D [
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.blackChecker; None; None];
            [None; None; None; None; None; None; Piece.whiteChecker; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    Assert.True(jumpAvailable Player.Black board)

[<Fact>]
let ``setPieceAt places piece``() =
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
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; None; None; Piece.blackKing; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    
    Assert.Equal(expectedBoard, board |> setPieceAt {Row = 1; Column = 4} Piece.blackKing)

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
    
    let newBoard = (board |> movePiece {Row = 0; Column = 1} {Row = 1; Column = 2}).Value
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
    
    let newBoard = (board |> movePiece {Row = 6; Column = 0} {Row = 7; Column = 1}).Value
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
    
    let newBoard = (board |> movePiece {Row = 1; Column = 1} {Row = 0; Column = 0}).Value
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
    
    let newBoard = (board |> movePiece {Row = 5; Column = 0} {Row = 7; Column = 2}).Value
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
    
    let newBoard = (board |> movePiece {Row = 2; Column = 2} {Row = 0; Column = 0}).Value
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
    
    let newBoard = (board |> movePiece {Row = 0; Column = 1} {Row = 2; Column = 3}).Value
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
    
    let newBoard = (board |> movePiece {Row = 0; Column = 2} {Row = 2; Column = 0}).Value
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
    
    let newBoard = (board |> movePiece {Row = 2; Column = 0} {Row = 0; Column = 2}).Value
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
    
    let newBoard = (board |> movePiece {Row = 2; Column = 2} {Row = 0; Column = 0}).Value
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
    
    let newBoard = (moveSequence [{Row = 0; Column = 0}; {Row = 2; Column = 2}; {Row = 0; Column = 4}] (Some <| board)).Value
    Assert.Equal(expectedBoard, newBoard)

[<Fact>]
let ``Player has move available``() =
    Assert.True(moveAvailable Board.defaultBoard White)

[<Fact>]
let ``Player with no pieces has no move available``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(moveAvailable board White)

[<Fact>]
let ``Player with pieces has no move available``() =
    let board =
        array2D [
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(moveAvailable board White)

[<Fact>]
let ``Player turn not ended``() =
    let originalBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; Piece.blackKing; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let newBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(playerTurnEnds [{ Row = 4; Column = 4 }; { Row = 2; Column = 2 }] originalBoard newBoard)

[<Fact>]
let ``Player turn does not end during flying king jump``() =
    let originalBoard = (controllerFromFen GameVariant.PoolCheckers "[FEN \"W:WK9:BK6,K11,K23\"]").Board
    let newBoard = (controllerFromFen GameVariant.PoolCheckers "[FEN \"W:WK2:BK11,K23\"]").Board

    Assert.False(playerTurnEnds [{ Row = 2; Column = 1 }; { Row = 0; Column = 3 }] originalBoard newBoard)

[<Fact>]
let ``Player turn does not end during flying king jump 1``() =
    let originalBoard = (controllerFromFen GameVariant.PoolCheckers "[FEN \"W:WK2:BK11,K23\"]").Board
    let newBoard = (controllerFromFen GameVariant.PoolCheckers "[FEN \"W:WK16:BK23\"]").Board

    Assert.False(playerTurnEnds [{ Row = 0; Column = 3 }; { Row = 3; Column = 6 }] originalBoard newBoard)

[<Fact>]
let ``Player turn not ended--checker piece``() =
    let originalBoard =
        array2D [
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let newBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.blackChecker; None; None; None; None; None];
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(playerTurnEnds [{ Row = 0; Column = 0 }; { Row = 2; Column = 2 }] originalBoard newBoard)

[<Fact>]
let ``Player turn ends when move is hop``() =
    let originalBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let newBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(playerTurnEnds [{ Row = 3; Column = 3 }; { Row = 2; Column = 2 }] originalBoard newBoard)

[<Fact>]
let ``Player turn ends when no jumps available``() =
    let originalBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.whiteChecker; None; None; None; None];
            [None; None; None; None; Piece.blackKing; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
        
    let newBoard =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(playerTurnEnds [{ Row = 4; Column = 4 }; { Row = 2; Column = 2 }] originalBoard newBoard)

[<Fact>]
let ``Winning player returns player``() =
    let board =
        array2D [
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.Equal(Black, (winningPlayer board None).Value)

[<Fact>]
let ``Winning player returns None``() =
    Assert.True((winningPlayer Board.defaultBoard None).IsNone)

[<Fact>]
let ``Winning player returns player when neither player can move``() =
    let fen = "[FEN \"W:W14,17,18,20,21,22,23,24,25,26,28,32:B5,6,7,8,9,10,11,12,13,15,16,19\"]"
    let controller = controllerFromFen GameVariant.PoolCheckers fen
    
    let board = Checkers.Variants.PoolCheckers.movePiece {Row = 7; Column = 6} {Row = 6; Column = 5} controller.Board

    Assert.Equal(White, (winningPlayer board.Value (Some White)).Value)