namespace AdventOfCode2022.Test

module StartMarker =
    open NUnit.Framework
    open AdventOfCode2022.StartMarker
    open AdventOfCode2022.Test.PuzzleInput

    let assertEqual expected actual =
        Assert.AreEqual(expected, actual)

    [<TestCase("bvwbjplbgvbhsrlpgdmjqwftvncz", 5)>]
    [<TestCase("nppdvjthqldpwncqszvftbrmjlhg", 6)>]
    [<TestCase("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10)>]
    [<TestCase("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)>]
    let findsMarkerForExamples datastream expected =
        let actual = findMarker datastream
        assertEqual expected actual

    [<Test>]
    let canSolveDay6Puzzle1 () =
        let actual = findMarker Puzzle6_Datastream
        printf $"Found marker at index {actual}"
