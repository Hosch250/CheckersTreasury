module Checkers.AIs.AmericanCheckersAI
open Checkers.Generic
open Checkers.Piece
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers

let checkerWeights =
    array2D [
        [0.0; 3.20; 0.0; 3.20; 0.0; 3.20; 0.0; 3.10];
        [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
        [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
        [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
        [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
        [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
        [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
        [3.10; 0.0; 3.20; 0.0; 3.20; 0.0; 3.20; 0.0]
    ]

let kingWeights =
    array2D [
        [0.0; 1.05; 0.0; 1.0; 0.0; 1.0; 0.0; 1.0];
        [1.05; 0.0; 1.10; 0.0; 1.05; 0.0; 1.05; 0.0];
        [0.0; 1.10; 0.0; 1.15; 0.0; 1.10; 0.0; 1.0];
        [1.0; 0.0; 1.15; 0.0; 1.20; 0.0; 1.05; 0.0];
        [0.0; 1.05; 0.0; 1.20; 0.0; 1.15; 0.0; 1.0];
        [1.0; 0.0; 1.10; 0.0; 1.15; 0.0; 1.10; 0.0];
        [0.0; 1.05; 0.0; 1.05; 0.0; 1.10; 0.0; 1.05];
        [1.0; 0.0; 1.0; 0.0; 1.0; 0.0; 1.05; 0.0]
    ]

let calculateCheckerWeight piece coord =
    let kingRow = kingRowIndex piece.Player

    let weight = 8.0 - (float <| abs(kingRow - coord.Row)) + (square coord checkerWeights)
    match piece.Player with
    | Black -> weight
    | White -> -weight

let calculateKingWeight piece coord =
    let weight = 8.0 + (square coord kingWeights)

    match piece.Player with
    | Black -> weight
    | White -> -weight

let calculatePieceWeight piece coord =
    match piece.PieceType with
    | Checker -> calculateCheckerWeight piece coord
    | King -> calculateKingWeight piece coord

let calculateWeight player (board :Board) =
    let rec loop (weight :float) coord :float =
        match nextPoint coord Rows Columns with
        | None -> weight
        | Some c ->
            let piece = square coord board
            match piece with
            | Some p when p.Player = player -> loop (weight + (calculatePieceWeight p coord)) c
            | _ -> loop weight c
    
    loop 0.0 {Row = 0; Column = 0}

let calculateWeightDifference (board :Board) =
    let rec loop (weight :float) coord =
        match nextPoint coord Rows Columns with
        | None -> weight
        | Some c ->
            let piece = square coord board
            match piece with
            | Some p -> loop (weight + (calculatePieceWeight p coord)) c
            | None -> loop weight c
    
    loop 0.0 {Row = 0; Column = 0}

let checkerJumps player =
    match player with
    | White -> [{Row = -2; Column = -2}; {Row = -2; Column = 2}]
    | Black -> [{Row = 2; Column = -2}; {Row = 2; Column = 2}]

let kingJumps player =
    (checkerJumps player) @
        (match player with
        | White -> [{Row = 2; Column = -2}; {Row = 2; Column = 2}]
        | Black -> [{Row = -2; Column = -2}; {Row = -2; Column = 2}])

let checkerHops player =
    match player with
    | White -> [{Row = -1; Column = -1}; {Row = -1; Column = 1}]
    | Black -> [{Row = 1; Column = -1}; {Row = 1; Column = 1}]

let kingHops player =
    (checkerHops player) @
        (match player with
        | White -> [{Row = 1; Column = -1}; {Row = 1; Column = 1}]
        | Black -> [{Row = -1; Column = -1}; {Row = -1; Column = 1}])

let getPieceSingleJumps coord (board :Board) =
    let piece = (square coord board).Value
    let moves = 
        match piece.PieceType with
        | Checker -> checkerJumps piece.Player
        | King -> kingJumps piece.Player

    let rec loop (acc :Move List) moves =
        let move::tail = moves
        let endCoord = offset coord move
        match coordExists endCoord && isValidJump coord endCoord board, tail with
        | true, [] -> acc @ (List.singleton [coord; endCoord])
        | true, _ -> loop (acc @ (List.singleton [coord; endCoord])) tail
        | false, [] -> acc
        | false, _ -> loop acc tail

    loop [] moves

let rec internal createMoveTree (move :Move) (board :Board) =
    {
        Move = move;
        Children =
            let newBoard =
                match move.Length with
                | 1 -> board
                | _ -> uncheckedMoveSequence move board

            let newJumps = getPieceSingleJumps (List.last move) newBoard
            let newMoveEndCoords = List.map (fun item -> List.last item) newJumps

            let oldPieceType = (square move.Head board).Value.PieceType
            let newPieceType = (square (List.last move) newBoard).Value.PieceType

            match newMoveEndCoords.IsEmpty || (oldPieceType = Checker && newPieceType = King) with
            | true -> None
            | false ->
                let moves = List.map (fun (item :Coord) -> move @ [item]) newMoveEndCoords
                let children = List.map (fun item -> createMoveTree item board) moves
                Some children
    }

let getPieceJumps coord (board :Board) =
    let moves = new System.Collections.Generic.List<Move>()

    let rec loop (moveTree :MoveTree) =
        match moveTree.Children with
        | None -> moves.Add(moveTree.Move)
        | Some t -> List.iter (fun item -> (loop item)) t

    let moveTree = createMoveTree [coord] board
    match moveTree.Children with
    | Some _ -> loop <| createMoveTree [coord] board
    | None -> ()

    List.ofSeq moves

let getPieceHops coord (board :Board) =
    let piece = (square coord board).Value
    let moves = 
        match piece.PieceType with
        | Checker -> checkerHops piece.Player
        | King -> kingHops piece.Player

    let hopsFilter = List.filter (fun (head::tail) ->
        let startCoord = head
        let endCoord = tail |> List.head
        coordExists endCoord && isValidHop startCoord endCoord board)

    moves |> List.map (fun move -> [coord; offset coord move]) |> hopsFilter

let calculateMoves requireJumps player (board :Board) =
    let rec loop jumpAcc hopAcc coord =
        match isPlayerPiece player coord board with
        | true ->
            let newJumpAcc = getPieceJumps coord board @ jumpAcc
            match newJumpAcc, requireJumps with
            | b, true when b <> [] ->
                match nextPoint coord Rows Columns with
                | Some c -> loop newJumpAcc [] c
                | None -> newJumpAcc
            | _ ->
                let newHopAcc = getPieceHops coord board @ hopAcc
                match nextPoint coord Rows Columns with
                | Some c -> loop newJumpAcc newHopAcc c
                | None -> newJumpAcc @ newHopAcc
        | false ->
            match nextPoint coord Rows Columns with
            | Some c -> loop jumpAcc hopAcc c
            | None -> jumpAcc @ hopAcc
    
    loop [] [] {Row = 0; Column = 0}