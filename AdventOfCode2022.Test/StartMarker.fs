namespace AdventOfCode2022.Test

module StartMarker =
    open NUnit.Framework
    open AdventOfCode2022.Test.Functions
    open AdventOfCode2022.StartMarker
    open AdventOfCode2022.Test.PuzzleInput

    [<TestCase("bvwbjplbgvbhsrlpgdmjqwftvncz", 5)>]
    [<TestCase("nppdvjthqldpwncqszvftbrmjlhg", 6)>]
    [<TestCase("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10)>]
    [<TestCase("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)>]
    let findsStartOfPacketMarkerForExamples datastream expected =
        let actual = findStartOfPacketMarker datastream
        assertEqual expected actual

    [<TestCase("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19)>]
    [<TestCase("bvwbjplbgvbhsrlpgdmjqwftvncz", 23)>]
    [<TestCase("nppdvjthqldpwncqszvftbrmjlhg", 23)>]
    [<TestCase("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29)>]
    [<TestCase("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)>]
    let findsStartOfMessageMarkerForExamples datastream expected =
        let actual = findStartOfMessageMarker datastream
        assertEqual expected actual

    [<Test>]
    let canSolveDay6Puzzle1 () =
        let actual = findStartOfPacketMarker Puzzle6_Datastream
        printf $"Found start-of-packet marker at index {actual}"

    [<Test>]
    let canSolveDay6Puzzle2 () =
        let actual = findStartOfMessageMarker Puzzle6_Datastream
        printf $"Found start-of-message marker at index {actual}"
