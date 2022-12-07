namespace AdventOfCode2022.Test

module Functions =
    open NUnit.Framework

    let assertEqual expected actual =
        Assert.AreEqual(expected, actual)
