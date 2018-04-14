module Checkers.PortableDraughtsNotation
open Checkers.Generic
open Checkers.Piece
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.GameVariant
open Checkers.GameController
open System

[<Literal>]
let BlackSymbol = 'B'

[<Literal>]
let WhiteSymbol = 'W'

let internal getPieceNotation (fenSections :string[]) (playerSymbol :char) =
    (match fenSections.[1].[0] with
     | c when c = playerSymbol -> fenSections.[1]
     | _ -> fenSections.[2])
         .Remove(0, 1)
         .Split([|','|], StringSplitOptions.RemoveEmptyEntries)

let rec addPieces (fenPieces :string List) player (board :Board) (pdnBoardCoords :Coord List) =
    let (head::tail) = fenPieces
    let isKing = head.[0] = 'K'
    let fenNumber =
        match isKing with
        | true -> Int32.Parse(head.Remove(0, 1))
        | false -> Int32.Parse(head)

    let boardCoord = pdnBoardCoords.[fenNumber]
    board.[boardCoord.Row, boardCoord.Column] <-
        match (player, isKing) with
        | (White, true) -> whiteKing
        | (White, false) -> whiteChecker
        | (Black, true) -> blackKing
        | (Black, false) -> blackChecker
    
    if not tail.IsEmpty then addPieces tail player board pdnBoardCoords

let controllerFromFen variant (fen :string) =
    let fenValue = fen.Split('"').[1]
    let fenSubsections = fenValue.Split(':')
    let playerTurn =
        match fenSubsections.[0].[0] with
        | BlackSymbol -> Black
        | WhiteSymbol -> White
        
    let whitePieces = getPieceNotation fenSubsections WhiteSymbol
    let blackPieces = getPieceNotation fenSubsections BlackSymbol

    let pdnBoardCoords = variant.pdnMembers.pdnBoardCoords
    
    let board = emptyBoardList()
    if whitePieces.Length > 0 then addPieces (List.ofArray whitePieces) White board pdnBoardCoords
    if blackPieces.Length > 0 then addPieces (List.ofArray blackPieces) Black board pdnBoardCoords

    {
        Variant = variant
        Board = board;
        CurrentPlayer = playerTurn;
        InitialPosition = fen;
        MoveHistory =
            match playerTurn with
            | White ->
                [{MoveNumber = 1; BlackMove = {Move = []; ResultingFen = fen; DisplayString = "…"; PieceTypeMoved = None; Player = None; IsJump = None}; WhiteMove = None}]
            | Black -> []
        CurrentCoord = None;
    }

let createFen variant player (board :Board) =
    let turnSymbol =
        match player with
        | White -> WhiteSymbol
        | Black -> BlackSymbol

    let rec loop (fenNumbers :string List) player coord =
        match nextPoint coord 7 7 with
        | Some c ->
            let piece = square coord board
            match piece.IsSome && isPlayerPiece player coord board with
            | true ->
                let isKing = piece.Value.PieceType = King

                let pdnBoard = variant.pdnBoard

                let fenNumber = (square coord pdnBoard).Value
                loop (fenNumbers @ [(if isKing then "K" else "") + fenNumber.ToString()]) player c
            | false -> loop fenNumbers player c
        | None -> fenNumbers

    let whitePieceFEN = String.Join(",", (loop [] White {Row = 0; Column = 0}))
    let blackPieceFEN = String.Join(",", (loop [] Black {Row = 0; Column = 0}))

    "[FEN \"" + turnSymbol.ToString() + ":W" + whitePieceFEN + ":B" + blackPieceFEN + "\"]"