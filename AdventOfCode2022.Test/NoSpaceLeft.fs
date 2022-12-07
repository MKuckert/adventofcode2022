namespace AdventOfCode2022.Test

module NoSpaceLeft =
    open NUnit.Framework
    open AdventOfCode2022.Test.Functions
    open AdventOfCode2022.NoSpaceLeft
    open AdventOfCode2022.Test.PuzzleInput

    let exampleInput = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

    [<Test>]
    let canParseExample () =
        let lines = parse exampleInput
        assertEqual 23 lines.Length
        assertEqual (Command (ChangeDirectory "/")) lines[0]
        assertEqual (Command List) lines[1]
        assertEqual (Directory "a") lines[2]
        assertEqual (File (File.create "b.txt" 14848514u)) lines[3]

    [<Test>]
    let canLayoutExample () =
        let layout = parse exampleInput |> layout
        assertEqual "a" layout.Directories[1].Name
        assertEqual "d" layout.Directories[2].Name
        assertEqual "e" layout.Directories[3].Name

        let d = layout.Directories[Layout.directoryNamed layout "d"]
        assertEqual "d.log" d.Files[1].Name
        assertEqual 8033020 d.Files[1].Size

    [<Test>]
    let canFindWithMaxSizeInExample () =
        let directories = parse exampleInput
                          |> layout
                          |> directoriesWithMaxSize 100000u
        assertEqual 2 directories.Length
        assertEqual "a" directories[0].Name
        assertEqual "e" directories[1].Name

    [<Test>]
    let canSolveDay7Puzzle1 () =
        let layout = parse Puzzle7_Directories |> layout
        let size = layout
                   |> directoriesWithMaxSize 100000u
                   |> List.sumBy (fun dir -> Directory.size layout dir)
        
        printf $"Total size of at most 100000 is {size}"
