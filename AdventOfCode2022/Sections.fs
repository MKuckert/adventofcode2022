namespace AdventOfCode2022

module Sections =

    open System

    module List =
        let containsAll (first: 'T list) (second: 'T list) =
            first
            |> List.forall (fun item -> List.contains item second)

        let overlaps (first: 'T list) (second: 'T list) =
            let laps = first
                       |> List.filter (fun item -> List.contains item second)
            laps.Length > 0

    type ElfPair = { First: int list; Second: int list }

    type ElfPair with
        static member readRange(input: string) =
            let parts = input.Split("-")
                        |> Seq.map int
                        |> List.ofSeq
            [ for i in parts[0] .. parts[1] -> i ]

        static member read(input: string) =
            let ranges = input.Split(",")
            { First = ElfPair.readRange ranges[0]
              Second = ElfPair.readRange ranges[1] }

        static member contained pair =
            List.containsAll pair.First pair.Second
            || List.containsAll pair.Second pair.First

        static member overlapping pair =
            List.overlaps pair.First pair.Second
            || List.overlaps pair.Second pair.First

    let readRangePairs (input: string) : ElfPair list =
        input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map ElfPair.read
        |> List.ofSeq
