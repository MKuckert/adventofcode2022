namespace AdventOfCode2022

module StartMarker =
    [<Literal>]
    let private MarkerSize = 4
    let private isMarker (input: char[]) =
        (input |> Array.distinct).Length = MarkerSize

    let findMarker (input: string) =
        let windowIndex = input.ToCharArray()
                          |> Seq.windowed MarkerSize
                          |> Seq.findIndex isMarker
        windowIndex + MarkerSize
