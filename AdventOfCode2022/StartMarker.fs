namespace AdventOfCode2022

module StartMarker =
    [<Literal>]
    let private StartOfPacketMarkerSize = 4

    [<Literal>]
    let private StartOfMessageMarkerSize = 14

    let private isMarker (input: char[]) =
        (input |> Array.distinct).Length = input.Length

    let private findMarker (input: string) marker =
        let windowIndex =
            input.ToCharArray()
            |> Seq.windowed marker
            |> Seq.findIndex isMarker

        windowIndex + marker

    let findStartOfPacketMarker (input: string) =
        findMarker input StartOfPacketMarkerSize

    let findStartOfMessageMarker (input: string) =
        findMarker input StartOfMessageMarkerSize
