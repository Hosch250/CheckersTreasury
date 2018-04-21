module public Checkers.PublicAPI
open Checkers.Generic
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.GameController
open Checkers.PortableDraughtsNotation
open Checkers.GameVariant
open Checkers.Minimax
open System

let getGameVariant variant =
    match variant with
    | AmericanCheckers -> GameVariant.AmericanCheckers
    | PoolCheckers -> GameVariant.PoolCheckers

let pdnBoard variant =
    variant.pdnBoard

let pdnBoardCoords variant =
    variant.pdnBoardCoords

let isValidMove startCoord endCoord gameController =
    gameController.Variant.apiMembers.isValidMove startCoord endCoord gameController.Board &&
    (square startCoord gameController.Board).Value.Player = gameController.CurrentPlayer &&
    match gameController.CurrentCoord with
    | None -> true
    | coord -> startCoord = coord.Value

let internal getDisplayString variant (pdnTurn :int List) (move :Move) (board :Board) =
    String.Join((if variant.isJump move board then "x" else "-"), pdnTurn)
    
let internal getPdnForMove gameController move boardFen originalBoard =
    let gameHistory = gameController.MoveHistory
    let pdnMove = (List.map (fun item -> (square item gameController.Variant.pdnMembers.pdnBoard).Value) move)
    
    let lastTurn = List.tryLast gameHistory
    let moveNumber =
        match gameController.CurrentPlayer with
        | Black -> gameHistory.Length + 1
        | White -> gameHistory.Length

    let piece :Checkers.Piece.Piece Option = square move.Head originalBoard

    let blackMove =
        match gameController.CurrentPlayer with
        | Black ->
            Some {
                Move = pdnMove;
                ResultingFen = boardFen;
                DisplayString = getDisplayString gameController.Variant.apiMembers pdnMove move originalBoard;
                PieceTypeMoved = Some piece.Value.PieceType;
                Player = Some Black;
                IsJump = Some (gameController.Variant.apiMembers.isJump move originalBoard)
            }
        | White ->
            match lastTurn.IsSome && lastTurn.Value.MoveNumber = moveNumber with
            | true -> (List.last gameHistory).BlackMove
            | false -> None

    let whiteMove =
        match gameController.CurrentPlayer with
        | Black ->
            match lastTurn.IsSome && lastTurn.Value.MoveNumber = moveNumber with
            | true -> (List.last gameHistory).WhiteMove
            | false -> None
        | White ->
            Some 
                {
                    Move = pdnMove;
                    ResultingFen = boardFen;
                    DisplayString = getDisplayString gameController.Variant.apiMembers pdnMove move originalBoard;
                    PieceTypeMoved = Some piece.Value.PieceType;
                    Player = Some White;
                    IsJump = Some (gameController.Variant.apiMembers.isJump move originalBoard)
                }

    {MoveNumber = moveNumber; BlackMove = blackMove; WhiteMove = whiteMove}
    
let internal getPdnForContinuedMove gameController move boardFen originalBoard =
    let gameHistory = gameController.MoveHistory
    
    let lastMovePdn = List.last gameHistory
    let pdnMove = (List.map (fun item -> (square item gameController.Variant.pdnMembers.pdnBoard).Value) move)

    let moveNumber = lastMovePdn.MoveNumber

    let piece :Checkers.Piece.Piece Option = square move.Head originalBoard

    let blackMove =
        match gameController.CurrentPlayer with
        | Black ->
            let newPdnMove = lastMovePdn.BlackMove.Value.Move @ pdnMove.Tail
            Some {
                Move = newPdnMove;
                ResultingFen = boardFen;
                DisplayString = getDisplayString gameController.Variant.apiMembers newPdnMove move originalBoard;
                PieceTypeMoved = Some piece.Value.PieceType;
                Player = Some Black;
                IsJump = Some (gameController.Variant.apiMembers.isJump move originalBoard)
            }
        | White -> lastMovePdn.BlackMove

    let whiteMove =
        match gameController.CurrentPlayer with
        | Black -> lastMovePdn.WhiteMove
        | White ->
            let newPdnMove = lastMovePdn.WhiteMove.Value.Move @ pdnMove.Tail
            Some
                {
                    Move = newPdnMove;
                    ResultingFen = boardFen;
                    DisplayString = getDisplayString gameController.Variant.apiMembers newPdnMove move originalBoard;
                    PieceTypeMoved = Some piece.Value.PieceType;
                    Player = Some White;
                    IsJump = Some (gameController.Variant.apiMembers.isJump move originalBoard)
                }

    {MoveNumber = moveNumber; BlackMove = blackMove; WhiteMove = whiteMove}

let internal getGameHistory gameController move boardFen originalBoard =
    let isContinuedMove = gameController.CurrentCoord <> None

    let newTurnValue =
        match isContinuedMove with
        | false -> getPdnForMove gameController move boardFen originalBoard
        | true -> getPdnForContinuedMove gameController move boardFen originalBoard

    match gameController.CurrentPlayer, isContinuedMove with
    | Black, false -> gameController.MoveHistory @ [newTurnValue]
    | _ ->
        match gameController.MoveHistory with
        | [] -> [newTurnValue]
        | _ -> (List.take (gameController.MoveHistory.Length - 1) gameController.MoveHistory) @ [newTurnValue]

let movePiece startCoord endCoord gameController :Option<GameController> =
    let originalBoard = gameController.Board
    let board = gameController.Variant.apiMembers.movePiece startCoord endCoord gameController.Board

    match board with
    | None -> None
    | Some b ->
        let isTurnEnding = gameController.Variant.apiMembers.playerTurnEnds [startCoord; endCoord] gameController.Board b
        let nextPlayerTurn = 
            match gameController.Variant.apiMembers.playerTurnEnds [startCoord; endCoord] gameController.Board b with
            | true -> otherPlayer gameController.CurrentPlayer
            | false -> gameController.CurrentPlayer

        Some <|
            {
                Variant = gameController.Variant
                Board = b
                CurrentPlayer = nextPlayerTurn
                InitialPosition = gameController.InitialPosition
                MoveHistory = getGameHistory gameController [startCoord; endCoord] (createFen gameController.Variant.pdnMembers nextPlayerTurn b) originalBoard
                CurrentCoord = if isTurnEnding then None else Some endCoord
            }

let move (move :Coord seq) (gameController) :Option<GameController> =
    let originalBoard = gameController.Board
    let board = gameController.Variant.apiMembers.moveSequence move (Some gameController.Board)
    let moveAsList = List.ofSeq move

    match board with
    | None -> None
    | Some b ->
        let isTurnEnding = gameController.Variant.apiMembers.playerTurnEnds moveAsList gameController.Board b
        let nextPlayerTurn =
            match gameController.Variant.apiMembers.playerTurnEnds moveAsList gameController.Board b with
            | true -> otherPlayer gameController.CurrentPlayer
            | false -> gameController.CurrentPlayer

        Some <|
            {
                Variant = gameController.Variant
                Board = b;
                CurrentPlayer = nextPlayerTurn
                InitialPosition = gameController.InitialPosition
                MoveHistory = getGameHistory gameController moveAsList (createFen gameController.Variant.pdnMembers nextPlayerTurn b) originalBoard
                CurrentCoord = if isTurnEnding then None else Some (Seq.last move)
            }

let getMove searchDepth gameController (cancellationToken :System.Threading.CancellationToken) =
    (minimax gameController.CurrentPlayer searchDepth searchDepth None None gameController.Board gameController.Variant.aiMembers cancellationToken).Move

let getValidMoves gameController =
    gameController.Variant.aiMembers.calculateMoves gameController.CurrentPlayer gameController.Board

let takeBackMove (gameController :GameController) =
    let whiteMoves =
        List.map (fun (item :PdnTurn) -> item.WhiteMove.Value) (List.filter (fun (item :PdnTurn) -> item.WhiteMove.IsSome) gameController.MoveHistory)
        
    let blackMoves =
        List.map (fun (item :PdnTurn) -> item.BlackMove.Value) (List.filter (fun (item :PdnTurn) -> item.BlackMove.IsSome) gameController.MoveHistory)

    let fen =
        match gameController.CurrentPlayer with
        | Black when blackMoves.Length > 0 -> (blackMoves.[blackMoves.Length - 1]).ResultingFen
        | White when whiteMoves.Length > 0 -> (whiteMoves.[whiteMoves.Length - 1]).ResultingFen
        | _ -> gameController.InitialPosition

    let newMoveHistory =
        match gameController.MoveHistory with
        | [] -> []
        | _ ->
            let lastTurn = List.last gameController.MoveHistory
            match lastTurn.BlackMove, lastTurn.WhiteMove with
            | Some _, Some _ -> List.truncate (gameController.MoveHistory.Length - 1) gameController.MoveHistory
            | _ when blackMoves.IsEmpty || whiteMoves.IsEmpty -> []
            | _ ->
                let lastMove = (List.last gameController.MoveHistory)
                let newLastMove =
                    match gameController.CurrentPlayer with
                    | Black -> {lastMove with WhiteMove = None}
                    | White -> {lastMove with BlackMove = None}
                List.truncate (gameController.MoveHistory.Length - 1) gameController.MoveHistory @ [newLastMove]       
    
    {(controllerFromFen gameController.Variant fen) with MoveHistory = newMoveHistory}

let winningPlayer controller =
    let lastTurn = List.tryLast controller.MoveHistory

    let lastPlayer =
        match lastTurn with
        | Some turn when turn.BlackMove = None -> Some White
        | Some turn when turn.WhiteMove = None -> Some Black
        | Some _ -> Some controller.Variant.apiMembers.startingPlayer
        | None -> None

    controller.Variant.apiMembers.winningPlayer controller.Board lastPlayer

let isWon controller =
    let player = winningPlayer controller
    player.IsSome &&
    player.Value <> controller.CurrentPlayer

let createFen variant player (board :Board) =
    createFen variant player board

let controllerFromFen variant fen =
    controllerFromFen variant fen

let isDrawn (controller :GameController) =
    controller.Variant.apiMembers.isDrawn controller.InitialPosition controller.MoveHistory