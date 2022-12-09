namespace AdventOfCode2022.Test

module TreetopTreeHouse =
    open NUnit.Framework
    open AdventOfCode2022.Test.Functions
    open AdventOfCode2022.TreetopTreeHouse
    open AdventOfCode2022.Test.PuzzleInput
    
    let exampleInput = """30373
25512
65332
33549
35390"""

    [<Test>]
    let canParseExampleInput () =
        let trees = parse exampleInput
        assertEqual 3 trees[0,0]
        assertEqual 0 trees[0,1]
        assertEqual 3 trees[0,2]
        assertEqual 9 trees[4,3]

    [<TestCase(0, 0, true)>] // edge
    [<TestCase(4, 4, true)>] // edge
    [<TestCase(1, 3, false)>] // 1
    [<TestCase(3, 3, false)>] // 3 in the middle
    [<TestCase(3, 4, true)>] // 3 right of the middle
    let isVisibleInExampleInput x y expected =
        let actual = parse exampleInput
                     |> isVisible x y
        assertEqual expected actual

    [<Test>]
    let canCountVisibleTreesInExampleInput () =
        let visible = parse exampleInput |> countVisibleTrees
        assertEqual 21 visible

    [<Test>]
    let canSolveDay8Puzzle1 () =
        let visible = parse Puzzle8_Trees |> countVisibleTrees
        printf $"Has {visible} trees"

    [<TestCase(1, 2, 4)>] // middle 5 in second row
    [<TestCase(3, 2, 8)>] // middle 5 in fourth row
    let scenicScoreInExampleInput x y expected =
        let actual = parse exampleInput
                     |> scenicScore x y
        assertEqual expected actual

    [<Test>]
    let canSolveDay8Puzzle2 () =
        let score = parse Puzzle8_Trees |> findHighestScenicScore 
        printf $"Highest scenic score is {score}"
