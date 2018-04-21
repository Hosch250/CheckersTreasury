module internal Checkers.Minimax
open Checkers.Generic
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.GameVariant
open System.Threading

let rec internal bestMatchInList player highestDifference moveForHighestDifference (list :List<float * Move>) =
    let head::tail = list
    let weight = fst head

    let newMoveForHighestDifference =
        match player with
        | Black -> match weight > highestDifference with
                   | true -> snd head
                   | false -> moveForHighestDifference
        | White -> match weight < highestDifference with
                   | true -> snd head
                   | false -> moveForHighestDifference

    let newHighestDifference =
        (highestDifference, weight)
        ||> match player with
            | Black -> max
            | White -> min

    match tail with
    | [] -> (highestDifference, newMoveForHighestDifference)
    | _ -> bestMatchInList player newHighestDifference newMoveForHighestDifference list.Tail

let internal chooseNewAlpha currentAlpha (candidateAlpha :float Option) =
    match (currentAlpha, candidateAlpha) with
    | (Some current, Some candidate) -> Some <| max current candidate
    | (Some current, None) -> Some current
    | (None, Some candidate) -> Some candidate
    | _ -> None

let internal chooseNewBeta currentBeta (candidateBeta :float Option) =
    match (currentBeta, candidateBeta) with
    | (Some current, Some candidate) -> Some <| min current candidate
    | (Some current, None) -> Some current
    | (None, Some candidate) -> Some candidate
    | _ -> None

let rec minimax player currentSearchDepth searchDepth alpha beta (board :Board) (aiMembers :AiMembers) (cancellationToken :CancellationToken) =
    match cancellationToken.IsCancellationRequested with
    | true -> { Alpha = None; Beta = None; Move = [] }
    | _ ->
        match currentSearchDepth = 0 || (aiMembers.winningPlayer board player).IsSome with
        | true ->
            let weightDifference = Some <| aiMembers.calculateWeightDifference board
            let newAlpha = 
                match player with
                | Black -> weightDifference
                | _ -> alpha
            let newBeta =
                match player with
                | White -> weightDifference
                | _ -> beta
            { Alpha = newBeta; Beta = newAlpha; Move = [] }
        | false ->
            let getNewValueAndMove chooseMethod nodeValue moveValue currentValue currentMove newMove =
                let newNodeValue = chooseMethod nodeValue moveValue
                let newValue = chooseMethod currentValue newNodeValue
                let finalMove = 
                    match newValue with
                    | a when a = moveValue -> currentMove
                    | _ -> newMove
                (newNodeValue, newValue, finalMove)

            let rec loop alphaForNode betaForNode (newAlpha :float Option) (newBeta :float Option) move moves =
                match moves with
                | [] -> { Alpha = betaForNode; Beta = alphaForNode; Move = move }
                | _ ->
                    let currentMove :Move = moves.Head
                    match newAlpha.IsNone || newBeta.IsNone || newAlpha.Value < newBeta.Value with
                    | false ->
                        match cancellationToken.IsCancellationRequested with
                        | true -> loop alphaForNode betaForNode newAlpha newBeta move (moves |> List.tail)
                        | false -> {Alpha = None; Beta = None; Move = []}
                    | true ->
                        let newBoard = aiMembers.uncheckedMoveSequence (Seq.ofList currentMove) board

                        let alphaBetaMove = minimax (otherPlayer player) (currentSearchDepth - 1) searchDepth alphaForNode betaForNode newBoard aiMembers cancellationToken

                        match cancellationToken.IsCancellationRequested with
                        | true -> {Alpha = None; Beta = None; Move = []}
                        | false ->
                            match player with
                            | Black ->
                                let (newAlphaForNode, newNewAlpha, newMove) = getNewValueAndMove chooseNewAlpha alphaForNode alphaBetaMove.Alpha newAlpha currentMove move
                                loop newAlphaForNode betaForNode newNewAlpha newBeta newMove (moves |> List.tail)
                            | White ->
                                let (newBetaForNode, newNewBeta, newMove) = getNewValueAndMove chooseNewBeta betaForNode alphaBetaMove.Beta newBeta currentMove move
                                loop alphaForNode newBetaForNode newAlpha newNewBeta newMove (moves |> List.tail)

            let potentialMoves = aiMembers.calculateMoves player board
            match cancellationToken.IsCancellationRequested with
            | true -> {Alpha = None; Beta = None; Move = []}
            | _ ->
                match potentialMoves.Length, currentSearchDepth = searchDepth with
                | 1, true -> {Alpha = None; Beta = None; Move = potentialMoves.[0]}
                | _ -> loop None None alpha beta [] potentialMoves