namespace AdventOfCode2022.Test

module RockPaperScissorsTournament =
    open NUnit.Framework
    open AdventOfCode2022.RockPaperScissorsTournament
    open AdventOfCode2022.Test.PuzzleInput

    let assertRound opponent response round =
        Assert.AreEqual(opponent, round.Opponent)
        Assert.AreEqual(response, round.Response)

    [<Test>]
    let CanReadSingleHandShapeStrategy () =
        let input = "A Y"

        let rounds = readStrategy input
        Assert.AreEqual(1, rounds.Length)
        assertRound Rock Paper rounds[0]

    [<Test>]
    let CanReadExampleStrategy () =
        let input =
            """A Y
B X
C Z
"""

        let rounds = readStrategy input
        Assert.AreEqual(3, rounds.Length)
        assertRound Rock Paper rounds[0]
        assertRound Paper Rock rounds[1]
        assertRound Scissors Scissors rounds[2]

    [<Test>]
    let CanReadPuzzleInput () =
        let rounds = readStrategy Puzzle2_EncryptedStrategyGuide
        Assert.AreEqual(2500, rounds.Length)
        assertRound Paper Scissors rounds[0]

    [<Test>]
    let CanCalculateTotalExampleScore () =
        let input =
            """A Y
B X
C Z
"""

        let rounds = readStrategy input
        let actual = totalScore rounds
        Assert.AreEqual(15, actual)

    [<Test>]
    let CanSolveDay2Puzzle1 () =
        let rounds = readStrategy Puzzle2_EncryptedStrategyGuide
        let score = totalScore rounds
        printfn $"Got a total score of %d{score} after playing %d{rounds.Length} rounds"
