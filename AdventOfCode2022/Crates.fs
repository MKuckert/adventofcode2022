namespace AdventOfCode2022

module Crates =

    open System.Text.RegularExpressions

    type Crate = { Name: char }
    type Stack = { Crates: Crate list }

    type Stack with

        member this.Length = this.Crates.Length

        static member topCrateNames stacks =
            stacks
            |> List.map (fun stack -> stack.Crates.Head.Name.ToString())
            |> String.concat ""

    type Movement = { Quantity: int; From: int; To: int }

    type CrateInput =
        | Crate of Crate: Crate
        | None

    type CrateInput with

        static member isCrate input =
            match input with
            | Crate _ -> true
            | _ -> false

        static member unwrap(input: CrateInput) =
            match input with
            | Crate c -> c
            | _ -> failwith "Can't unwrap non-crate input"

    type Line =
        | Crates of Crates: CrateInput list
        | Separator
        | Movement of Movement: Movement

    type Line with

        static member isCrate line =
            match line with
            | Crates _ -> true
            | _ -> false
        static member isMovement line =
            match line with
            | Movement _ -> true
            | _ -> false

        static member unwrapCrate input =
            match input with
            | Crates c -> c
            | _ -> failwith $"Can't unwrap non-crate line {input} as a crate"

        static member unwrapMovement input =
            match input with
            | Movement m -> m
            | _ -> failwith $"Can't unwrap non-movement line {input} as a movement"

    type Input =
        { Stacks: Stack list
          Movements: Movement list }

    type Crane =
        CrateMover9000
        | CrateMover9001
    type Crane with
        static member take crane crates =
            match crane with
            | CrateMover9000 -> List.rev crates
            | CrateMover9001 -> crates

    let crate name = { Name = name }
    let stack crates = { Crates = crates }

    let move quantity from to' =
        { Quantity = quantity
          From = from
          To = to' }

    let private regexMatchGroups input pattern =
        Regex.Match(input, pattern).Groups.Values
        |> Seq.skip 1
        |> Seq.map (fun g -> g.Value)
        |> List.ofSeq

    let private regexMatchAll input pattern =
        Regex.Matches(input, pattern) |> Seq.map (fun g -> g.Value)

    let private performOne (crane: Crane) (stacks: Stack list) (movement: Movement) =
        let to' = movement.To - 1
        let from = movement.From - 1
        let taken, remaining = List.splitAt movement.Quantity stacks[from].Crates
        let newTarget = stack (Crane.take crane taken @ stacks[to'].Crates)
        let newSource = stack remaining

        stacks |> List.updateAt to' newTarget |> List.updateAt from newSource

    let perform (crane: Crane) (stacks: Stack list) (movements: Movement list) =
        movements
        |> List.fold (performOne crane) stacks

    let private parseCrates (input: string) =
        regexMatchAll input "\[.\]|    ?"
        |> Seq.map (function
            | v when v[0]='[' -> Crate(crate v[1])
            | _ -> None)
        |> List.ofSeq

    let private parseMovement (input: string) =
        let m = regexMatchGroups input "move (\d+) from (\d+) to (\d+)"
        move (int m[0]) (int m[1]) (int m[2])

    let private parseLine (input: string) =
        if input.Contains('[') then
            Crates(parseCrates input)
        else if input.StartsWith("move") then
            Movement(parseMovement input)
        else
            Separator

    let parse (input: string) =
        let lines = input.Split("\n") |> Seq.map parseLine |> List.ofSeq

        let stacks =
            lines
            |> List.filter Line.isCrate
            |> List.map Line.unwrapCrate
            |> List.transpose
            |> List.map (List.filter CrateInput.isCrate)
            |> List.map (List.map CrateInput.unwrap)
            |> List.map stack

        let moves =
            lines
            |> List.filter Line.isMovement
            |> List.map Line.unwrapMovement

        { Stacks = stacks; Movements = moves }
