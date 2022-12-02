namespace AdventOfCode2022

module RockPaperScissorsTournament =
    open System
    open Microsoft.FSharp.Core

    type HandShape =
        | Rock
        | Paper
        | Scissors

    type RoundOutcome =
        | Lose
        | Draw
        | Win

    type RoundExpectation =
        { Opponent: HandShape
          ExpectedOutcome: RoundOutcome }

    type Score = uint

    let handShapeScore shape =
        match shape with
        | Rock -> 1u
        | Paper -> 2u
        | Scissors -> 3u

    let selectResponseForExpectation expectation =
        match expectation.Opponent, expectation.ExpectedOutcome with
        // draw
        | op, Draw -> op
        // win
        | Rock, Win -> Paper
        | Paper, Win -> Scissors
        | Scissors, Win -> Rock
        // lose
        | Rock, Lose -> Scissors
        | Paper, Lose -> Rock
        | Scissors, Lose -> Paper

    let outcomeScore round =
        match round.ExpectedOutcome with
        | Draw -> 3u
        | Win -> 6u
        | _ -> 0u

    let roundScore round : Score =
        let response = selectResponseForExpectation round
        handShapeScore response
        + outcomeScore round

    let totalScore (rounds: RoundExpectation list) =
        rounds
        |> List.sumBy roundScore

    let private readHandShape (input: char) : HandShape =
        match input with
        | 'A' -> Rock
        | 'B' -> Paper
        | 'C' -> Scissors
        | _ -> failwith $"Invalid hand shape input %c{input}"

    let private readOutcome (input: char) : RoundOutcome =
        match input with
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> failwith $"Invalid round outcome input %c{input}"

    let private readRoundSelection (input: string) : RoundExpectation =
        { Opponent = readHandShape input[0]
          ExpectedOutcome = readOutcome input[2] }

    let private readStrategyFromEncryptedGuide (input: string seq) : RoundExpectation list =
        input
        |> Seq.map readRoundSelection
        |> List.ofSeq

    let readStrategy (input: string) =
        readStrategyFromEncryptedGuide (input.Split("\n", StringSplitOptions.RemoveEmptyEntries))
