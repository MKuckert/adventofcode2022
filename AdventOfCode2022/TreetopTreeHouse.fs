namespace AdventOfCode2022

module TreetopTreeHouse =
    open System

    let private isEdge x y (trees: int[,]) =
        x = 0 || y = 0 || x = trees.GetLength(0) - 1 || y = trees.GetLength(1) - 1

    let isVisible x y (trees: int[,]) =
        let v = trees[x,y]

        isEdge x y trees
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
        array2Dfold (fun state array x y -> state + (countVisible x y array)) 0 trees

    let viewingDistance (reference: int) (view: int[]) =
        let index = view |> Array.tryFindIndex (fun o -> o >= reference)
        if index.IsNone then
            view.Length
        else
            (Option.defaultValue 0 index) + 1

    let scenicScore x y (trees: int[,]) =
        if isEdge x y trees then
            0
        else
            let v = viewingDistance trees[x,y]

            let viewLeft = trees[.. x - 1, y] |> Array.rev |> v
            let viewRight = trees[x + 1 .., y] |> v
            let viewUp = trees[x, .. y - 1] |> Array.rev |> v
            let viewDown = trees[x, y + 1 ..] |> v
            viewLeft * viewRight * viewUp * viewDown

    let findHighestScenicScore trees =
        array2Dfold (fun state array x y -> Math.Max(state, scenicScore x y array)) 0 trees

    let parse (input: string) =
        input.Split("\n")
        |> Seq.map (fun line -> line.ToCharArray()
                                |> Seq.map string
                                |> Seq.map Int32.Parse)
        |> array2D
