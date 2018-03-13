module public Checkers.Board
open Checkers.Generic
open Checkers.Piece

type Board = Piece Option [,]

let square coord (board :'a [,]) =
    board.[coord.Row, coord.Column]

let defaultBoard :Piece Option [,] =
    array2D [
        [None; blackChecker; None; blackChecker; None; blackChecker; None; blackChecker]
        [blackChecker; None; blackChecker; None; blackChecker; None; blackChecker; None]
        [None; blackChecker; None; blackChecker; None; blackChecker; None; blackChecker]
        [None; None; None; None; None; None; None; None]
        [None; None; None; None; None; None; None; None]
        [whiteChecker; None; whiteChecker; None; whiteChecker; None; whiteChecker; None]
        [None; whiteChecker; None; whiteChecker; None; whiteChecker; None; whiteChecker]
        [whiteChecker; None; whiteChecker; None; whiteChecker; None; whiteChecker; None]
    ]
    
let defaultFen = "[FEN \"B:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12\"]"

let emptyBoardList() =
    Array2D.create 8 8 Option<Piece>.None