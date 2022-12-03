namespace AdventOfCode2022

module Rucksacks =

    open System
    open Microsoft.FSharp.Core

    type Rucksack =
        { Description: string }
        member this.FirstCompartment
            with get () = this.Description.Substring(0, this.Description.Length/2)
        member this.SecondCompartment
            with get () = this.Description.Substring(this.Description.Length/2)

    type ElfGroup = Rucksack list

    [<Literal>]
    let ElfGroupSize = 3

    let itemPriority (item: char) =
        if Char.IsLower(item) then
            (int item) - (int 'a') + 1
        else
            (int item) - (int 'A') + 27

    type Rucksack with
        static member create input =
            { Description = input }

        static member findSharedItem(rucksack: Rucksack) =
            rucksack.FirstCompartment.ToCharArray()
            |> Array.filter rucksack.SecondCompartment.Contains
            |> Array.head

        static member findSharedItem(group: ElfGroup) =
            group[0].Description.ToCharArray()
            |> Array.filter group[1].Description.Contains
            |> Array.filter group[2].Description.Contains
            |> Array.head

        static member totalSharedItemPriority(rucksacks: Rucksack list) =
            rucksacks
            |> List.map Rucksack.findSharedItem
            |> List.map itemPriority
            |> List.sum
        
        static member findElfGroups(rucksacks: Rucksack list) : ElfGroup list =
            rucksacks
            |> List.chunkBySize ElfGroupSize

    let private readRucksackCompartments (input: string) =
        let len = input.Length / 2

        { Description = input }

    let readRucksack (input: string) =
        input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> List.ofSeq
        |> List.map Rucksack.create
