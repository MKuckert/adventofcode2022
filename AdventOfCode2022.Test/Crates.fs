namespace AdventOfCode2022.Test

module Crates =
    open Microsoft.FSharp.Core
    open NUnit.Framework
    open AdventOfCode2022.Crates
    open AdventOfCode2022.Test.PuzzleInput

    let exampleInput =
        """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""
    let exampleStacks = [
        stack [ crate 'N'; crate 'Z' ]
        stack [ crate 'D'; crate 'C'; crate 'M']
        stack [ crate 'P' ]
    ]
    let exampleMovements = [
        move 1 2 1
        move 3 1 3
        move 2 2 1
        move 1 1 2
    ]

    let assertEqual expected actual =
        Assert.AreEqual(expected, actual)

    let assertStack (expected: char list) (actual: Stack) =
        let expected' = expected
                        |> List.map crate
                        |> stack
        assertEqual expected' actual
    
    let assertMessage message stacks =
        let actual = Stack.topCrateNames stacks
        assertEqual message actual

    [<Test>]
    let canPerformMovement () =
        let stacks = [
            stack [ crate 'A' ]
            stack [ crate 'B' ]
        ]
        let movements = [
            move 1 1 2
        ]
        let stacks' : Stack list = perform stacks movements
        assertEqual 0 stacks'[0].Length
        assertEqual 2 stacks'[1].Length

    [<Test>]
    let canPerformSingleMovementFromExample () =
        let movements = [ exampleMovements[0] ]
        let stacks' : Stack list = perform exampleStacks movements
        assertEqual 3 stacks'[0].Length
        assertEqual 2 stacks'[1].Length
        assertEqual 1 stacks'[2].Length
        assertStack [ 'D'; 'N'; 'Z' ] stacks'[0]
        assertStack [ 'C'; 'M' ] stacks'[1]
        assertStack [ 'P' ] stacks'[2]

    [<Test>]
    let canPerformMovementsFromExample () =
        let stacks' : Stack list = perform exampleStacks exampleMovements
        assertEqual 1 stacks'[0].Length
        assertEqual 1 stacks'[1].Length
        assertEqual 4 stacks'[2].Length
        assertStack [ 'C' ] stacks'[0]
        assertStack [ 'M' ] stacks'[1]
        assertStack [ 'Z'; 'N'; 'D'; 'P' ] stacks'[2]
        assertMessage "CMZ" stacks'

    [<Test>]
    let canParseExample () =
        let actual = parse exampleInput
        let expected = {
            Stacks = exampleStacks
            Movements = exampleMovements
        }
        assertEqual expected actual

    [<Test>]
    let canParseInput () =
        let actual = parse Puzzle5_Crates
        assertEqual 9 actual.Stacks.Length
        assertStack ['C'; 'S'; 'G'; 'B'] actual.Stacks[0]

    [<Test>]
    let canSolveDay5Puzzle1 () =
        let actual = parse Puzzle5_Crates
        let result = perform actual.Stacks actual.Movements
        printf $"Resulting message is {Stack.topCrateNames result}"
