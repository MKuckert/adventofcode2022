namespace AdventOfCode2022.Test

module Sections =

    open NUnit.Framework
    open AdventOfCode2022.Sections
    open AdventOfCode2022.Test.PuzzleInput
    
    let exampleInput = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""

    let assertPair firstFrom firstTo secondFrom secondTo pair =
        Assert.AreEqual(firstFrom, pair.First.Head)
        Assert.AreEqual(firstTo, pair.First[pair.First.Length - 1])
        Assert.AreEqual(secondFrom, pair.Second.Head)
        Assert.AreEqual(secondTo, pair.Second[pair.Second.Length - 1])

    [<Test>]
    let ListContains () =
        Assert.IsFalse(List.containsAll [ 1; 2; 3 ] [ 4; 5; 6 ])
        Assert.IsTrue(List.containsAll [ 2; 3 ] [ 1; 2; 3 ])
        Assert.IsFalse(List.containsAll [ 1; 2; 3 ] [ 2; 3; 4 ])

    [<Test>]
    let CanReadExampleInput () =
        let pairs = readRangePairs exampleInput
        Assert.AreEqual(6, pairs.Length)
        assertPair 2 4 6 8 pairs[0]
        assertPair 2 3 4 5 pairs[1]
        assertPair 5 7 7 9 pairs[2]
        assertPair 2 8 3 7 pairs[3]
        assertPair 6 6 4 6 pairs[4]
        assertPair 2 6 4 8 pairs[5]

    [<Test>]
    let CanFindContainedPairsInExampleInput () =
        let containedPairs = readRangePairs exampleInput
                             |> List.filter ElfPair.contained
        Assert.AreEqual(2, containedPairs.Length)

    [<Test>]
    let CanSolveDay4Puzzle1 () =
        let containedPairs = readRangePairs Puzzle4_Sections
                             |> List.filter ElfPair.contained
        printf $"Has {containedPairs.Length} pairs containing the other one"
