namespace AdventOfCode2022.Test

module Rucksacks =

    open NUnit.Framework
    open AdventOfCode2022.Rucksacks
    open AdventOfCode2022.Test.PuzzleInput

    let assertRucksack first second rucksack =
        Assert.AreEqual(first, rucksack.FirstCompartment)
        Assert.AreEqual(second, rucksack.SecondCompartmant)

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
        let input = """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

        let total = readRucksack input
                    |> Rucksack.totalSharedItemPriority
        Assert.AreEqual(157, total)

    [<Test>]
    let CanSolveDay3Puzzle1 () =
        let total = readRucksack Puzzle3_Rucksacks
                    |> Rucksack.totalSharedItemPriority
        printf $"Total priority for all shared items is %d{total}"
