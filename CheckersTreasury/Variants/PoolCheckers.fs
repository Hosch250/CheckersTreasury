module internal Checkers.Variants.PoolCheckers
open Checkers.Generic
open Checkers.Piece
open Checkers.Board
open Checkers.FSharpExtensions
open System

[<Literal>]
let Rows = 7
    
[<Literal>]
let Columns = 7
    
let StartingPlayer = Black

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
    { Row = endCoord.Row + Math.Sign(startCoord.Row - endCoord.Row); Column = endCoord.Column + Math.Sign(startCoord.Column - endCoord.Column) }

let internal kingRowIndex(player) =
    match player with
    | Black -> Rows
    | White -> 0
    
let internal coordExists coord =
    coord.Row >= 0 && coord.Row <= Rows &&
    coord.Column >= 0 && coord.Column <= Columns
    
let internal isJump (move :Move) (originalBoard :Board) =
    match abs (move.[0].Row - move.[1].Row) with
    | 1 -> false
    | _ ->
        let rowSign = Math.Sign(move.[0].Row - move.[1].Row)
        let colSign = Math.Sign(move.[0].Column - move.[1].Column)

        let targetCoord = {Row = (List.last move).Row + rowSign; Column = (List.last move).Column + colSign}

        match coordExists targetCoord with
        | false -> false
        | true ->
            match square targetCoord originalBoard with
            | None -> false
            | _ -> true

let rec getJumpTarget currentPlayer currentCoord rowSign colSign (board :Board) =
    let nextCoord = offset currentCoord {Row = rowSign; Column = colSign}
    match currentCoord, nextCoord with
    | _, c when not <| coordExists c -> None
    | cc, cn when ((square cc board).IsSome && (square cc board).Value.Player = currentPlayer) || ((square cc board).IsSome && (square cn board).IsSome) -> None
    | cc, cn when (square cc board).IsSome && (square cc board).Value.Player <> currentPlayer && (square cn board).IsNone -> Some (offset currentCoord {Row = rowSign; Column = colSign})
    | _ -> getJumpTarget currentPlayer nextCoord rowSign colSign board

let getHopTargets currentPlayer currentCoord rowSign colSign (board :Board) =
    let rec getTargets targets currentPlayer currentCoord rowSign colSign (board :Board) =
        match currentCoord with
        | c when not <| coordExists c || (square c board).IsSome -> targets
        | _ ->
            let newTargets = targets @ [currentCoord]
            let nextCoord = offset currentCoord {Row = rowSign; Column = colSign}
            getTargets newTargets currentPlayer nextCoord rowSign colSign board

    getTargets [] currentPlayer currentCoord rowSign colSign board

let internal checkMoveDirection piece startCoord endCoord (board :Board) =
    let isJump = isJump [startCoord; endCoord] board
    match piece.PieceType, piece.Player, isJump with
    | Checker, Black, false -> startCoord.Row < endCoord.Row
    | Checker, White, false -> startCoord.Row > endCoord.Row
    | _, _, true | King, _, _ -> true

let internal isValidCheckerHop startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value
    
    abs (startCoord.Row - endCoord.Row) = 1 &&
    checkMoveDirection piece startCoord endCoord board &&
    (square endCoord board).IsNone
    
let internal isValidKingHop startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value
    let rowSign = Math.Sign(endCoord.Row - startCoord.Row)
    let colSign = Math.Sign(endCoord.Column - startCoord.Column)
    let nextCoord = offset startCoord {Row = rowSign; Column = colSign}

    let hopTargets = getHopTargets piece.Player nextCoord rowSign colSign board
    
    List.contains endCoord hopTargets

let internal isValidCheckerJump startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value
    
    let jumpedCoord = getJumpedCoord startCoord endCoord
    let jumpedPiece = square jumpedCoord board

    abs (startCoord.Row - endCoord.Row) = 2 &&
    (square endCoord board).IsNone &&
    jumpedPiece.IsSome &&
    jumpedPiece.Value.Player <> piece.Player

let internal isValidKingJump startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value

    let jumpedCoord = getJumpedCoord startCoord endCoord
    let jumpedPiece = square jumpedCoord board

    let rowSign = Math.Sign(endCoord.Row - startCoord.Row)
    let colSign = Math.Sign(endCoord.Column - startCoord.Column)
    let nextCoord = offset startCoord {Row = rowSign; Column = colSign}

    let jumpTarget = getJumpTarget piece.Player nextCoord rowSign colSign board

    jumpTarget.IsSome &&
    jumpTarget.Value = endCoord &&
    (square endCoord board).IsNone &&
    jumpedPiece.IsSome &&
    jumpedPiece.Value.Player <> piece.Player
    
let internal isValidHop startCoord endCoord (board :Board) =
    match (square startCoord board).Value.PieceType with
    | Checker -> isValidCheckerHop startCoord endCoord board
    | King -> isValidKingHop startCoord endCoord board
        
let internal isValidJump startCoord endCoord (board :Board) =
    match (square startCoord board).Value.PieceType with
    | Checker -> isValidCheckerJump startCoord endCoord board
    | King -> isValidKingJump startCoord endCoord board

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

let internal hasValidCheckerJump startCoord (board :Board) =
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

let internal hasValidKingJump startCoord (board :Board) =
    let jumpCoordOffsets =
        [
            {Row = -1; Column = 1};
            {Row = -1; Column = -1};
            {Row = 1; Column = 1};
            {Row = 1; Column = -1}
        ]

    let currentPlayer = (square startCoord board).Value.Player

    let rec getJumps acc jumpOffsets =
        let head::tail = jumpOffsets
        let jumpCoord = getJumpTarget currentPlayer (offset startCoord {Row = head.Row; Column = head.Column}) head.Row head.Column board
        let currentJumps =
            match jumpCoord with
            | None -> acc
            | _ -> acc @ [jumpCoord.Value]

        match tail with
        | [] -> currentJumps
        | _ -> getJumps currentJumps tail
    
    not (getJumps [] jumpCoordOffsets).IsEmpty

let internal hasValidJump startCoord (board :Board) =
    let piece = square startCoord board
    match piece.Value.PieceType with
    | Checker -> hasValidCheckerJump startCoord board
    | King -> hasValidKingJump startCoord board

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

let winningPlayer (board :Board) (currentPlayer :Player Option) =
    match (moveAvailable board) with
    | x when not <| x Black && not <| x White -> currentPlayer
    | x when not <| x White -> Some Black
    | x when not <| x Black -> Some White
    | _ -> None

let playerHasOneKing (fen :string) =
    let fenValue = fen.Split('"').[1]
    let fenSubsections = fenValue.Split(':')
    
    let whitePieces = fenSubsections.[1]
                        .Remove(0, 1)
                        .Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        
    let blackPieces = fenSubsections.[2]
                        .Remove(0, 1)
                        .Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                        
    match whitePieces, blackPieces with
    | whitePieces, _ when whitePieces.Length = 1 && whitePieces.[0].[0] = 'K' -> true
    | _ , blackPieces when blackPieces.Length = 1 && blackPieces.[0].[0] = 'K' -> true
    | _ -> false
    
let isDrawn initialFen (moveHistory :PdnTurn list) =
    let fens =
        initialFen :: List.collect (fun item ->
            (
            match item.BlackMove, item.WhiteMove with
            | Some blackMove, Some whiteMove when not blackMove.Move.IsEmpty && not whiteMove.Move.IsEmpty -> [blackMove.ResultingFen; whiteMove.ResultingFen]
            | Some blackMove, whiteMove when not blackMove.Move.IsEmpty && (whiteMove.IsNone || whiteMove.IsSome && whiteMove.Value.Move.IsEmpty) -> [blackMove.ResultingFen]
            | blackMove, Some whiteMove when (blackMove.IsNone || blackMove.IsSome && blackMove.Value.Move.IsEmpty) && not whiteMove.Move.IsEmpty -> [whiteMove.ResultingFen]
            | _ -> []
            )) moveHistory
    let positionsByTimesReached = List.groupBy (fun item -> item) fens
    let hasReachedPositionThreeTimes = List.exists (fun (_, (values :string list)) -> values.Length >= 3) positionsByTimesReached

    let playerWithOneKing = List.skipWhile (fun item -> not (playerHasOneKing item)) fens

    hasReachedPositionThreeTimes || playerWithOneKing.Length = 26

let internal setPieceAt coord piece (board :Board) =
    let newBoard = Array2D.copy board
    
    newBoard.[coord.Row, coord.Column] <- piece
    newBoard

let internal playerTurnEnds (move :Move) (originalBoard :Board) (currentBoard :Board) =
    let lastMoveWasJump = isJump move originalBoard
    let pieceWasPromoted = (square (List.last move) currentBoard).Value.PieceType = King &&
                            (square move.[0] originalBoard).Value.PieceType = Checker

    pieceWasPromoted ||
    not (lastMoveWasJump && hasValidJump (List.last move) currentBoard)

let internal jump startCoord endCoord (board :Board) =
    let kingRowIndex = kingRowIndex((square startCoord board).Value.Player)

    let piece = (square startCoord board)
    let jumpedCoord = getJumpedCoord startCoord endCoord

    let currentBoard =
        board
        |> setPieceAt startCoord None
        |> setPieceAt endCoord piece
        |> setPieceAt jumpedCoord None

    if playerTurnEnds [startCoord; endCoord] board currentBoard && endCoord.Row = kingRowIndex then
        setPieceAt endCoord (Some <| Promote piece.Value) currentBoard
    else
        currentBoard

let internal hop startCoord endCoord (board :Board) =
    let kingRowIndex = kingRowIndex (square startCoord board).Value.Player

    let piece =
        match endCoord.Row with
        | row when row = kingRowIndex -> Some <| Promote (square startCoord board).Value
        | _ -> (square startCoord board)
            
    board
    |> setPieceAt startCoord None
    |> setPieceAt endCoord piece

let public isValidMove startCoord endCoord (board :Board) =
    coordExists startCoord &&
    coordExists endCoord &&
    moveIsDiagonal startCoord endCoord &&
    (square startCoord board).IsSome &&
    ((isValidHop startCoord endCoord board && not <| jumpAvailable (square startCoord board).Value.Player board) ||
     isValidJump startCoord endCoord board)

let public movePiece startCoord endCoord (board :Board) :Option<Board> =
    match isValidMove startCoord endCoord board with
    | false -> None
    | true ->
        match abs(startCoord.Row - endCoord.Row) with
        | 1 -> Some <| hop startCoord endCoord board
        | _ -> Some <| jump startCoord endCoord board

let rec public moveSequence (coordinates :Coord seq) (board :Option<Board>) =
    let coords = List.ofSeq(coordinates)

    match board with
    | None -> None
    | _ ->
        match coords.Length with
        | c when c >= 3 ->
            let newBoard = movePiece coords.Head coords.[1] board.Value
            moveSequence coords.Tail newBoard
        | _ -> movePiece coords.Head coords.[1] board.Value

let internal uncheckedMovePiece startCoord endCoord (board :Board) =
    match abs(startCoord.Row - endCoord.Row) with
    | 1 -> hop startCoord endCoord board
    | _ -> jump startCoord endCoord board

let rec internal uncheckedMoveSequence (coordinates :Coord seq) (board :Board) =
    let coords = List.ofSeq(coordinates)

    match coords.Length with
    | b when b >= 3 ->
        let newBoard = uncheckedMovePiece coords.Head coords.[1] board
        uncheckedMoveSequence coords.Tail newBoard
    | _ -> uncheckedMovePiece coords.Head coords.[1] board