namespace AdventOfCode2022.Test

module Rucksacks =

    open NUnit.Framework
    open AdventOfCode2022.Rucksacks
    open AdventOfCode2022.Test.PuzzleInput

    let assertRucksack first second (rucksack: Rucksack) =
        Assert.AreEqual(first, rucksack.FirstCompartment)
        Assert.AreEqual(second, rucksack.SecondCompartment)
    let exampleInput = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

    [<Test>]
    let CanReadSingleRucksack () =
        let input = "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"

        let rucksacks = readRucksack input
        Assert.AreEqual(1, rucksacks.Length)
        assertRucksack "jqHRNqRjqzjGDLGL" "rsFMfFZSrLrFZsSL" rucksacks[0]

    [<Test>]
    [<TestCase("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", 'L')>]
    [<TestCase("vJrwpWtwJgWrhcsFMMfFFhFp", 'p')>]
    let CanFindSharedItemInRucksack (input, expected) =
        let shared = readRucksack input
                     |> List.head
                     |> Rucksack.findSharedItem
        Assert.AreEqual(expected, shared)

    [<Test>]
    [<TestCase('a', 1)>]
    [<TestCase('z', 26)>]
    [<TestCase('A', 27)>]
    [<TestCase('Z', 52)>]
    let CanGetItemPriority (item, prio) =
        Assert.AreEqual(prio, itemPriority item)

    [<Test>]
    let CanSumTotalPriorityForExample () =
        let total = readRucksack exampleInput
                    |> Rucksack.totalSharedItemPriority
        Assert.AreEqual(157, total)

    [<Test>]
    let CanSolveDay3Puzzle1 () =
        let total = readRucksack Puzzle3_Rucksacks
                    |> Rucksack.totalSharedItemPriority
        printf $"Total priority of all shared items is %d{total}"

    [<Test>]
    let CanFindElfGroups () =
        let groups = readRucksack exampleInput
                    |> Rucksack.findElfGroups

        Assert.AreEqual(2, groups.Length)
        Assert.AreEqual("vJrwpWtwJgWr", groups[0].[0].FirstCompartment)
        Assert.AreEqual("wMqvLMZHhHMvwLH", groups[1].[0].FirstCompartment)

    [<Test>]
    let CanFindSharedItemForElfGroups () =
        let sharedItems = readRucksack exampleInput
                          |> Rucksack.findElfGroups
                          |> List.map Rucksack.findSharedItem

        Assert.AreEqual(2, sharedItems.Length)
        Assert.AreEqual('r', sharedItems[0])
        Assert.AreEqual('Z', sharedItems[1])

    [<Test>]
    let CanSolveDay3Puzzle2 () =
        let total = readRucksack Puzzle3_Rucksacks
                    |> Rucksack.findElfGroups
                    |> List.map Rucksack.findSharedItem
                    |> List.map itemPriority
                    |> List.sum
        printf $"Total priority of all elf group badges is %d{total}"
