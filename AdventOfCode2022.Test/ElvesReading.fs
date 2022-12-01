module AdventOfCode2022.Test

open NUnit.Framework
open AdventOfCode2022.Elves
open AdventOfCode2022.PuzzleInput

let assertFood (calories: uint list) (elf: Elf) =
    let foodList =
        calories |> List.map (fun item -> { Calories = Calories.toCalories item })

    CollectionAssert.AreEqual(foodList, elf.CarriedFood)

[<Test>]
let CanReadSingleElf () =
    let input = "1"

    let elves = readElves input
    Assert.AreEqual(1, elves.Length)
    assertFood [ 1u ] elves[0]

[<Test>]
let CanReadElvesFromSampleInput () =
    let input =
        """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

    let elves = readElves input
    Assert.AreEqual(5, elves.Length)
    assertFood [ 1000u; 2000u; 3000u ] elves[0]
    assertFood [ 4000u ] elves[1]
    assertFood [ 5000u; 6000u ] elves[2]
    assertFood [ 7000u; 8000u; 9000u ] elves[3]
    assertFood [ 10000u ] elves[4]

[<Test>]
let CanGetSingleElfTotalCalories () =
    let elf = { CarriedFood = [ { Calories = (Calories.toCalories 1u) } ] }
    Assert.AreEqual(1, Elf.totalCalories elf)

[<Test>]
let CanGetSingleElfMultipleFoodTotalCalories () =
    let elf =
        { CarriedFood =
            [ { Calories = (Calories.toCalories 1u) }
              { Calories = (Calories.toCalories 2u) }
              { Calories = (Calories.toCalories 3u) }
              { Calories = (Calories.toCalories 4u) } ] }

    Assert.AreEqual(10, Elf.totalCalories elf)

[<Test>]
let CanGetSingleElfMaxTotalCalories () =
    let elves = [ { CarriedFood = [ { Calories = (Calories.toCalories 1u) } ] } ]
    Assert.AreEqual(1, Elf.maxTotalCalories elves)

[<Test>]
let CanGetMultipleElvesMaxTotalCalories () =
    let elves =
        [ { CarriedFood = [ { Calories = (Calories.toCalories 1u) } ] }
          { CarriedFood = [ { Calories = (Calories.toCalories 2u) } ] }
          { CarriedFood = [ { Calories = (Calories.toCalories 3u) } ] }
          { CarriedFood = [ { Calories = (Calories.toCalories 4u) } ] } ]

    Assert.AreEqual(4, Elf.maxTotalCalories elves)

[<Test>]
let CanSolveDay1Puzzle1 () =
    let elves = readElves Puzzle1_ElvesCalories
    printfn $"Max total calories out of %d{elves.Length} elves: %d{Elf.maxTotalCalories elves}"

[<Test>]
let CanSolveDay1Puzzle2 () =
    let totalCalories = readElves Puzzle1_ElvesCalories
                        |> Elf.filterHighestTotalCalories 3
                        |> Elf.sumTotalCalories
    
    printfn $"Summed total calories for the top 3 elves: %d{totalCalories}"
