module Checkers.GameVariant
open Checkers.Generic
open Checkers.Board

type AiMembers =
    {
        uncheckedMoveSequence :Coord seq -> Board -> Board
        calculateMoves :Player -> Board -> Move List
        winningPlayer :Board -> Player Option
        calculateWeightDifference :Board -> float
    }
with
    static member AmericanCheckers =
        {
            uncheckedMoveSequence = Variants.AmericanCheckers.uncheckedMoveSequence
            calculateMoves = AIs.AmericanCheckersAI.calculateMoves
            winningPlayer = Variants.AmericanCheckers.winningPlayer
            calculateWeightDifference = AIs.AmericanCheckersAI.calculateWeightDifference
        }
    static member PoolCheckers =
        {
            uncheckedMoveSequence = Variants.PoolCheckers.uncheckedMoveSequence
            calculateMoves = AIs.PoolCheckersAI.calculateMoves
            winningPlayer = Variants.PoolCheckers.winningPlayer
            calculateWeightDifference = AIs.PoolCheckersAI.calculateWeightDifference
        }

type PdnMembers =
    {
        pdnBoard :int Option [,]
        pdnBoardCoords :Coord List
    }
with
    static member AmericanCheckers =
        {
            pdnBoard = Variants.AmericanCheckers.pdnBoard
            pdnBoardCoords = Variants.AmericanCheckers.pdnBoardCoords
        }
    static member PoolCheckers =
        {
            pdnBoard = Variants.PoolCheckers.pdnBoard
            pdnBoardCoords = Variants.PoolCheckers.pdnBoardCoords
        }

type ApiMembers =
    {
        isValidMove :Coord -> Coord -> Board -> bool
        movePiece :Coord -> Coord -> Board -> Board Option
        moveSequence :Coord seq -> Board Option -> Board Option
        isJump :Move -> Board -> bool
        winningPlayer :Board -> Player Option
        isDrawn : string -> PdnTurn list -> bool
        playerTurnEnds :Move -> Board -> Board -> bool
    }
with
    static member AmericanCheckers =
        {
            isValidMove = Variants.AmericanCheckers.isValidMove
            movePiece = Variants.AmericanCheckers.movePiece
            moveSequence = Variants.AmericanCheckers.moveSequence
            isJump = Variants.AmericanCheckers.isJump
            winningPlayer = Variants.AmericanCheckers.winningPlayer
            isDrawn = Variants.AmericanCheckers.isDrawn
            playerTurnEnds = Variants.AmericanCheckers.playerTurnEnds
        }
    static member PoolCheckers =
        {
            isValidMove = Variants.PoolCheckers.isValidMove
            movePiece = Variants.PoolCheckers.movePiece
            moveSequence = Variants.PoolCheckers.moveSequence
            isJump = Variants.PoolCheckers.isJump
            winningPlayer = Variants.PoolCheckers.winningPlayer
            isDrawn = Variants.PoolCheckers.isDrawn
            playerTurnEnds = Variants.PoolCheckers.playerTurnEnds
        }

type GameVariant =
    {
        variant :Variant
        aiMembers :AiMembers
        pdnMembers :PdnMembers
        apiMembers :ApiMembers
    }
with
    static member AmericanCheckers =
        {
            variant = AmericanCheckers
            aiMembers = AiMembers.AmericanCheckers
            pdnMembers = PdnMembers.AmericanCheckers
            apiMembers = ApiMembers.AmericanCheckers
        }
    static member PoolCheckers =
        {
            variant = PoolCheckers
            aiMembers = AiMembers.PoolCheckers
            pdnMembers = PdnMembers.PoolCheckers
            apiMembers = ApiMembers.PoolCheckers
        }