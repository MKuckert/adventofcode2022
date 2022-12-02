namespace AdventOfCode2022

module RockPaperScissorsTournament =
    open System
    open Microsoft.FSharp.Core

    type HandShape =
        | Rock
        | Paper
        | Scissors

    type RoundSelection =
        { Opponent: HandShape
          Response: HandShape }

    type Score = uint
    
    let handShapeScore shape =
        match shape with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let private readHandShape (input: char) : HandShape =
        match input with
        | 'A' | 'X' -> Rock
        | 'B' | 'Y' -> Paper
        | 'C' | 'Z' -> Scissors
        | _ -> failwith $"Invalid hand shape input %c{input}"

    let private readRoundSelection (input: string) : RoundSelection =
        { Opponent = readHandShape input[0]
          Response = readHandShape input[2] }

    let private readStrategyFromEncryptedGuide (input: string seq) : RoundSelection list =
        input
        |> Seq.map readRoundSelection
        |> List.ofSeq

    let readStrategy (input: string) =
        readStrategyFromEncryptedGuide (input.Split("\n", StringSplitOptions.RemoveEmptyEntries))
