module internal Checkers.Variants.AmericanCheckers
open Checkers.Generic
open Checkers.Piece
open Checkers.Board
open Checkers.FSharpExtensions
open System

[<Literal>]
let Rows = 7
    
[<Literal>]
let Columns = 7

let internal pdnBoard =
    array2D [
        [None; Some 1; None; Some 2; None; Some 3; None; Some 4];
        [Some 5; None; Some 6; None; Some 7; None; Some 8; None];
        [None; Some 9; None; Some 10; None; Some 11; None; Some 12];
        [Some 13; None; Some 14; None; Some 15; None; Some 16; None];
        [None; Some 17; None; Some 18; None; Some 19; None; Some 20];
        [Some 21; None; Some 22; None; Some 23; None; Some 24; None];
        [None; Some 25; None; Some 26; None; Some 27; None; Some 28];
        [Some 29; None; Some 30; None; Some 31; None; Some 32; None];
    ]

let internal pdnBoardCoords =
    [
        {Row = -1; Column = -1};    // adjust for FEN's 1-based indexing
        {Row = 0; Column = 1}; {Row = 0; Column = 3}; {Row = 0; Column = 5}; {Row = 0; Column = 7};
        {Row = 1; Column = 0}; {Row = 1; Column = 2}; {Row = 1; Column = 4}; {Row = 1; Column = 6};
        {Row = 2; Column = 1}; {Row = 2; Column = 3}; {Row = 2; Column = 5}; {Row = 2; Column = 7};
        {Row = 3; Column = 0}; {Row = 3; Column = 2}; {Row = 3; Column = 4}; {Row = 3; Column = 6};
        {Row = 4; Column = 1}; {Row = 4; Column = 3}; {Row = 4; Column = 5}; {Row = 4; Column = 7};
        {Row = 5; Column = 0}; {Row = 5; Column = 2}; {Row = 5; Column = 4}; {Row = 5; Column = 6};
        {Row = 6; Column = 1}; {Row = 6; Column = 3}; {Row = 6; Column = 5}; {Row = 6; Column = 7};
        {Row = 7; Column = 0}; {Row = 7; Column = 2}; {Row = 7; Column = 4}; {Row = 7; Column = 6};
    ]

let internal getJumpedCoord startCoord endCoord =
    { Row = startCoord.Row - Math.Sign(startCoord.Row - endCoord.Row); Column = startCoord.Column - Math.Sign(startCoord.Column - endCoord.Column) }

let internal kingRowIndex(player) =
    match player with
    | Player.Black -> Rows
    | Player.White -> 0
    
let internal coordExists coord =
    coord.Row >= 0 && coord.Row <= Rows &&
    coord.Column >= 0 && coord.Column <= Columns
    
let internal isJump (move :Move) (originalBoard :Board) =
    match abs (move.[0].Row - move.[1].Row) with
    | 2 -> true
    | 1 -> false

let internal checkMoveDirection piece startCoord endCoord =
    match piece.PieceType with
    | PieceType.Checker ->
        match piece.Player with
        | Player.Black -> startCoord.Row < endCoord.Row
        | Player.White -> startCoord.Row > endCoord.Row
    | PieceType.King -> true

let internal isValidCheckerHop startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value

    checkMoveDirection piece startCoord endCoord &&
    (square endCoord board).IsNone

let internal isValidKingHop endCoord (board :Board) =
    (square endCoord board).IsNone

let internal isValidCheckerJump startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value
        
    let jumpedCoord = getJumpedCoord startCoord endCoord
    let jumpedPiece = square jumpedCoord board
        
    checkMoveDirection piece startCoord endCoord &&
    (square endCoord board).IsNone &&
    jumpedPiece.IsSome &&
    jumpedPiece.Value.Player <> piece.Player

let internal isValidKingJump startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value

    let jumpedCoord = getJumpedCoord startCoord endCoord
    let jumpedPiece = square jumpedCoord board

    (square endCoord board).IsNone &&
    jumpedPiece.IsSome &&
    jumpedPiece.Value.Player <> piece.Player
            
let internal isValidHop startCoord endCoord (board :Board) =
    match (square startCoord board).Value.PieceType with
    | PieceType.Checker -> isValidCheckerHop startCoord endCoord board
    | PieceType.King -> isValidKingHop endCoord board
        
let internal isValidJump startCoord endCoord (board :Board) =
    match (square startCoord board).Value.PieceType with
    | PieceType.Checker -> isValidCheckerJump startCoord endCoord board
    | PieceType.King -> isValidKingJump startCoord endCoord board

let internal hasValidHop startCoord (board :Board) =
    let hopCoords =
        [
            offset startCoord {Row = -1; Column = 1};
            offset startCoord {Row = -1; Column = -1};
            offset startCoord {Row = 1; Column = 1};
            offset startCoord {Row = 1; Column = -1}
        ]

    let rec anyHopIsValid hops =
        let coord::tail = hops
        match coordExists coord && isValidHop startCoord coord board, tail with
        | true, _ -> true
        | false, [] -> false
        | false, _ -> anyHopIsValid tail
            
    anyHopIsValid hopCoords

let internal hasValidJump startCoord (board :Board) =
    let jumpCoords =
        [
            offset startCoord {Row = -2; Column = 2};
            offset startCoord {Row = -2; Column = -2};
            offset startCoord {Row = 2; Column = 2};
            offset startCoord {Row = 2; Column = -2}
        ]

    let rec anyJumpIsValid jumps =
        let coord::tail = jumps
        match coordExists coord && isValidJump startCoord coord board, tail with
        | true, _ -> true
        | false, [] -> false
        | false, _ -> anyJumpIsValid tail
            
    anyJumpIsValid jumpCoords

let internal jumpAvailable player (board :Board) =
    let pieceHasJump row column =
        let piece = board.[row, column]
        piece.IsSome && piece.Value.Player = player && hasValidJump { Row = row; Column = column } board

    let rec loop coord =
        match coord with
        | None -> false
        | Some c ->
            match pieceHasJump c.Row c.Column with
            | true -> true
            | false -> loop (nextPoint c Rows Columns)

    loop <| Some {Row = 0; Column = 0}

let internal moveAvailable (board :Board) player =
    let pieceHasMove row column =
        let piece = board.[row, column]
        piece.IsSome &&
        piece.Value.Player = player &&
        (hasValidJump { Row = row; Column = column } board || hasValidHop { Row = row; Column = column } board)

    let rec loop coord =
        match coord with
        | None -> false
        | Some c ->
            match pieceHasMove c.Row c.Column with
            | true -> true
            | false -> loop (nextPoint c Rows Columns)

    loop <| Some {Row = 0; Column = 0}

let winningPlayer (board :Board) =
    match (moveAvailable board) with
    | x when not <| x White -> Some Black
    | x when not <| x Black -> Some White
    | _ -> None

let wasCheckerMoved moves =
    List.exists (fun (item :PdnMove) -> item.PieceTypeMoved.Value = PieceType.Checker) moves

let wasPieceJumped moves =
    List.exists (fun (item :PdnMove) -> item.IsJump.Value) moves

let isDrawn initialFen (moveHistory :PdnTurn list) =
    let fens =
        initialFen :: List.collect (fun item ->
            (
            match item.BlackMove, item.WhiteMove with
            | blackMove, Some whiteMove when not blackMove.Move.IsEmpty && not whiteMove.Move.IsEmpty -> [blackMove.ResultingFen; whiteMove.ResultingFen]
            | blackMove, whiteMove when not blackMove.Move.IsEmpty && (whiteMove.IsNone || whiteMove.IsSome && whiteMove.Value.Move.IsEmpty) -> [blackMove.ResultingFen]
            | blackMove, Some whiteMove when blackMove.Move.IsEmpty && not whiteMove.Move.IsEmpty -> [whiteMove.ResultingFen]
            | _ -> []
            )) moveHistory
    let positionsByTimesReached = List.groupBy (fun item -> item) fens
    let hasReachedPositionThreeTimes = List.exists (fun (_, (values :string list)) -> values.Length >= 3) positionsByTimesReached

    let whiteMoves =
        List.map (fun (item :PdnTurn) -> item.WhiteMove.Value) (List.filter (fun (item :PdnTurn) -> item.WhiteMove.IsSome) moveHistory)
        
    let blackMoves =
        List.map (fun (item :PdnTurn) -> item.BlackMove) moveHistory

    let lastFortyWhiteMoves =
        List.skip (whiteMoves.Length - 40) (List.filter (fun (item :PdnMove) -> not item.Move.IsEmpty) whiteMoves)

    let lastFortyBlackMoves =
        List.skip (blackMoves.Length - 40) (List.filter (fun (item :PdnMove) -> not item.Move.IsEmpty) blackMoves)

    hasReachedPositionThreeTimes ||
    (
    lastFortyWhiteMoves.Length = 40 &&
    lastFortyBlackMoves.Length = 40 &&
    not (wasCheckerMoved lastFortyWhiteMoves) &&
    not (wasCheckerMoved lastFortyBlackMoves) &&
    not (wasPieceJumped lastFortyWhiteMoves) &&
    not (wasPieceJumped lastFortyBlackMoves)
    )

let internal setPieceAt coord piece (board :Board) =
    let newBoard = Array2D.copy board
    
    newBoard.[coord.Row, coord.Column] <- piece
    newBoard

let internal jump startCoord endCoord (board :Board) =
    let kingRowIndex = kingRowIndex((square startCoord board).Value.Player)

    let piece =
        match endCoord.Row with
        | row when row = kingRowIndex -> Some <| Promote (square startCoord board).Value
        | _ -> (square startCoord board)

    let jumpedCoord = getJumpedCoord startCoord endCoord

    board
    |> setPieceAt startCoord None
    |> setPieceAt endCoord piece
    |> setPieceAt jumpedCoord None

let internal hop startCoord endCoord (board :Board) =
    let kingRowIndex = kingRowIndex (square startCoord board).Value.Player

    let piece =
        match endCoord.Row with
        | row when row = kingRowIndex -> Some <| Promote (square startCoord board).Value
        | _ -> (square startCoord board)
            
    board
    |> setPieceAt startCoord None
    |> setPieceAt endCoord piece

let internal playerTurnEnds (move :Move) (originalBoard :Board) (currentBoard :Board) =
    let lastMoveWasJump = abs(move.[0].Row - move.[1].Row) = 2
    let pieceWasPromoted = (square (List.last move) currentBoard).Value.PieceType = King &&
                            (square move.[0] originalBoard).Value.PieceType = Checker

    pieceWasPromoted ||
    not (lastMoveWasJump && hasValidJump (List.last move) currentBoard)

let public isValidMove startCoord endCoord (board :Board) =
    coordExists startCoord &&
    coordExists endCoord &&
    moveIsDiagonal startCoord endCoord &&
    (square startCoord board).IsSome &&
    match abs(startCoord.Row - endCoord.Row) with
    | 1 -> isValidHop startCoord endCoord board && not <| jumpAvailable (square startCoord board).Value.Player board
    | 2 -> isValidJump startCoord endCoord board
    | _ -> false

let public movePiece startCoord endCoord (board :Board) :Option<Board> =
    match isValidMove startCoord endCoord board with
    | false -> None
    | true ->
        match abs(startCoord.Row - endCoord.Row) with
        | 1 -> Some <| hop startCoord endCoord board
        | 2 -> Some <| jump startCoord endCoord board
        | _ -> None

let rec public moveSequence (coordinates :Coord seq) (board :Option<Board>) =
    let coords = List.ofSeq(coordinates)

    match board with
    | None -> None
    | Some b ->
        match coords.Length with
        | b when b >= 3 ->
            let newBoard = movePiece coords.Head coords.[1] board.Value
            moveSequence coords.Tail newBoard
        | _ -> movePiece coords.Head coords.[1] board.Value

let internal uncheckedMovePiece startCoord endCoord (board :Board) =
    match abs(startCoord.Row - endCoord.Row) with
    | 1 -> hop startCoord endCoord board
    | 2 -> jump startCoord endCoord board

let rec internal uncheckedMoveSequence (coordinates :Coord seq) (board :Board) =
    let coords = List.ofSeq(coordinates)

    match coords.Length with
    | b when b >= 3 ->
        let newBoard = uncheckedMovePiece coords.Head coords.[1] board
        uncheckedMoveSequence coords.Tail newBoard
    | _ -> uncheckedMovePiece coords.Head coords.[1] board