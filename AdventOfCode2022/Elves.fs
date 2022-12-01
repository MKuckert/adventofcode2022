namespace AdventOfCode2022

module Elves =
    open Microsoft.FSharp.Core

    // one would like to use Microsoft.FSharp.Data.UnitSystems.SI.UnitNames.joule
    // as a calorie is 4.1868 joule but there's no elegant way to create this as a derived unit
    [<Measure>]
    type Calories

    type Calories with

        static member toCalories(input: uint) = input * 1u<Calories>

    type Food = { Calories: uint<Calories> }
    type Elf = { CarriedFood: Food list }

    type Elf with

        static member totalCalories elf =
            elf.CarriedFood |> List.sumBy (fun food -> food.Calories)

        static member maxTotalCalories elves =
            elves
            |> List.map Elf.totalCalories
            |> List.max

    let private readCaloriesItem (input: string) : uint<Calories> option =
        match (input.Length, System.UInt32.TryParse(input)) with
        | 0, _ -> Option.None
        | _, (false, _) -> Option.None
        | _, (true, calories) -> Option.Some(Calories.toCalories calories)

    type private ElvesBuilder =
        { Elves: Elf list
          RemainingFood: Food list }

        static member empty =
            { Elves = List.empty
              RemainingFood = List.empty }

        static member addElf accum =
            let elf = { CarriedFood = accum.RemainingFood }

            { accum with
                Elves = (accum.Elves @ [ elf ])
                RemainingFood = List.empty }

        static member addFood accum food =
            { accum with RemainingFood = (accum.RemainingFood @ [ food ]) }

        static member build accum =
            // Add last elf for remaining food
            if accum.RemainingFood.Length > 0 then
                (ElvesBuilder.addElf accum).Elves
            else
                accum.Elves

    let private readElvesFromFoodList (input: string seq) =
        let foldFoodItem (accum: ElvesBuilder) (item: uint<Calories> option) : ElvesBuilder =
            match item with
            | Option.None ->
                // Empty food list item -> All remaining food belongs to a new elf
                ElvesBuilder.addElf accum
            | Option.Some calories ->
                // Non-empty food list item. This is for the next elf
                { Calories = calories } |> ElvesBuilder.addFood accum

        input
        |> Seq.map readCaloriesItem
        |> Seq.fold foldFoodItem ElvesBuilder.empty
        |> ElvesBuilder.build

    let readElves (input: string) =
        readElvesFromFoodList (input.Split("\n"))
