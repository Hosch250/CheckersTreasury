module internal Checkers.FSharpExtensions
open Checkers.Generic
open Checkers.Board

let internal moveIsDiagonal startCoord endCoord =
    startCoord <> endCoord &&
    abs (startCoord.Row - endCoord.Row) = abs (startCoord.Column - endCoord.Column)

let internal otherPlayer player =
    match player with
    | White -> Black
    | Black -> White

let isPlayerPiece player coord (board :Board) =
    let piece = square coord board
    piece.IsSome && player = piece.Value.Player

let nextPoint coord (rows :int) (columns :int) =
    match coord.Column with
    | column when column = columns ->
        match coord.Row with
        | row when row = rows -> None
        | row -> Some {Row = row + 1; Column = 0}
    | column -> Some {coord with Column = column + 1}