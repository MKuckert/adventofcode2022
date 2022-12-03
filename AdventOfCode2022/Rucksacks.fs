namespace AdventOfCode2022

module Rucksacks =

    open System
    open Microsoft.FSharp.Core

    type Rucksack =
        { FirstCompartment: string
          SecondCompartmant: string }

    let itemPriority (item: char) =
        if Char.IsLower(item) then
            (int item) - (int 'a') + 1
        else
            (int item) - (int 'A') + 27

    type Rucksack with
        static member findSharedItem(rucksack: Rucksack) =
            rucksack.FirstCompartment.ToCharArray()
            |> Array.filter rucksack.SecondCompartmant.Contains
            |> Array.head

        static member totalSharedItemPriority(rucksacks: Rucksack list) =
            rucksacks
            |> List.map Rucksack.findSharedItem
            |> List.map itemPriority
            |> List.sum

    let private readRucksackCompartments (input: string) =
        let len = input.Length / 2

        { FirstCompartment = input.Substring(0, len)
          SecondCompartmant = input.Substring(len) }

    let readRucksack (input: string) =
        input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> List.ofSeq
        |> List.map readRucksackCompartments
