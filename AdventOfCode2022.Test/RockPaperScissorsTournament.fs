namespace AdventOfCode2022.Test

module RockPaperScissorsTournament =
    open NUnit.Framework
    open AdventOfCode2022.RockPaperScissorsTournament
    open AdventOfCode2022.Test.PuzzleInput

    let assertRound (opponent: HandShape) (expectedOutcome: RoundOutcome) round =
        Assert.AreEqual(opponent, round.Opponent)
        Assert.AreEqual(expectedOutcome, round.ExpectedOutcome)

    [<Test>]
    let CanReadSingleHandShapeStrategy () =
        let input = "A Y"

        let rounds = readStrategy input
        Assert.AreEqual(1, rounds.Length)
        assertRound Rock Draw rounds[0]

    [<Test>]
    let CanReadExampleStrategy () =
        let input =
            """A Y
B X
C Z
"""

        let rounds = readStrategy input
        Assert.AreEqual(3, rounds.Length)
        assertRound Rock Draw rounds[0]
        assertRound Paper Lose rounds[1]
        assertRound Scissors Win rounds[2]

    [<Test>]
    let CanReadPuzzleInput () =
        let rounds = readStrategy Puzzle2_EncryptedStrategyGuide
        Assert.AreEqual(2500, rounds.Length)
        assertRound Paper Win rounds[0]

    [<Test>]
    let CanCalculateTotalExampleScore () =
        let input =
            """A Y
B X
C Z
"""

        let rounds = readStrategy input
        let actual = totalScore rounds
        Assert.AreEqual(12, actual)

    [<Test>]
    let CanSolveDay2Puzzle2 () =
        let rounds = readStrategy Puzzle2_EncryptedStrategyGuide
        let score = totalScore rounds
        printfn $"Got a total score of %d{score} after playing %d{rounds.Length} rounds"
