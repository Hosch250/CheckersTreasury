module public Checkers.GameController
open Checkers.Generic
open Checkers.Board
open Checkers.GameVariant

type GameController = { Variant :GameVariant; Board :Board; CurrentPlayer :Player; InitialPosition :string; MoveHistory :PdnTurn List; CurrentCoord :Option<Coord>; }
with
    static member newAmericanCheckersGame =
        { Variant = GameVariant.AmericanCheckers; Board = Checkers.Board.defaultBoard; CurrentPlayer = Black; InitialPosition = Checkers.Board.defaultFen; MoveHistory = []; CurrentCoord = None }
    static member newAmericanCheckersOptionalJumpGame =
        { Variant = GameVariant.AmericanCheckersOptionalJump; Board = Checkers.Board.defaultBoard; CurrentPlayer = Black; InitialPosition = Checkers.Board.defaultFen; MoveHistory = []; CurrentCoord = None }
    static member newPoolCheckersGame =
        { Variant = GameVariant.PoolCheckers; Board = Checkers.Board.defaultBoard; CurrentPlayer = Black; InitialPosition = Checkers.Board.defaultFen; MoveHistory = []; CurrentCoord = None }
