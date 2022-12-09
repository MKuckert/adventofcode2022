namespace AdventOfCode2022

module TreetopTreeHouse =
    open System

    let private isEdge (trees: int[,]) x y =
        x = 0 || y = 0 || x = trees.GetLength(0) - 1 || y = trees.GetLength(1) - 1

    let isVisible (trees: int[,]) x y =
        let v = trees[x,y]

        isEdge trees x y
        || trees[.. x - 1, y] |> Array.max < v
        || trees[x + 1 .., y] |> Array.max < v
        || trees[x, .. y - 1] |> Array.max < v
        || trees[x, y + 1 ..] |> Array.max < v

    let array2Dfold<'T, 'State> folder (state: 'State) (array: 'T[,])  =
        let mutable state' = state
        for x in 0..array.GetLength(0)-1 do
            for y in 0..array.GetLength(1)-1 do
                state' <- folder state' array x y
        state'

    let countVisibleTrees (trees: int[,]) =
        let countVisible array x y = if isVisible array x y then 1 else 0
        array2Dfold (fun state array x y -> state + (countVisible array x y)) 0 trees

    let parse (input: string) =
        input.Split("\n")
        |> Seq.map (fun line -> line.ToCharArray()
                                |> Seq.map string
                                |> Seq.map Int32.Parse)
        |> array2D
