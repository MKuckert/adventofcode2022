namespace AdventOfCode2022

module NoSpaceLeft =
    type File = {
        Name: string
        Size: uint
    }
    type File with
        static member create name size =
            { Name = name
              Size = size }

    type Directory = {
        Name: string
        Files: File list
        Parent: int
        Subs: int list
    }

    type Layout = {
        Directories: Directory list
        Current: int
    }

    type Directory with
        static member empty name parent =
            { Name = name
              Files = List.empty<File>
              Parent = parent
              Subs = List.empty<int> }
        member this.withSub sub =
            { this with Subs = this.Subs @ [sub] }
        member this.withFile file =
            { this with Files = this.Files @ [file] }

        static member size (layout: Layout) (dir: Directory) =
            let files = dir.Files
                        |> List.sumBy (fun file -> file.Size)
            let dirs = dir.Subs
                        |> List.sumBy (fun sub -> Directory.size layout (layout.Directories[sub]))
            files + dirs

    type Layout with
        member this.root =
            this.Directories[0]

        static member currentDirectory layout =
            layout.Directories[layout.Current]
        
        static member directoryNamed layout (name: string) =
            let cur = Layout.currentDirectory layout
            cur.Subs
            |> List.find (fun index -> layout.Directories[index].Name.Equals(name))

        static member withDirectory dir layout =
            { layout
              with Directories = layout.Directories
                                 @ [dir] }

        static member updatedCurrent dir layout =
            { layout
              with Directories = layout.Directories
                                 |> List.updateAt layout.Current dir }

        static member withCurrent index layout =
            { layout with Current = index }

        static member empty =
            {
                Directories = [ Directory.empty "/" 0 ]
                Current = 0 }

    type Command =
        ChangeDirectory of RelativePath: string
        | List

    type Line =
        Command of Command: Command
        | Directory of Name: string
        | File of File: File

    [<Literal>]
    let private CommandChangeDirectoryIndicator = "$ cd "
    [<Literal>]
    let private CommandListIndicator = "$ ls"

    [<Literal>]
    let private DirectoryIndicator = "dir "

    [<Literal>]
    let private NavigateUp = ".."

    let private parseFile (input: string) =
        let parts = input.Split(" ")
        File.create parts[1] (uint parts[0])

    let parse (input: string) =
        input.Split("\n")
        |> Seq.map (
            function
            | line when line.StartsWith(CommandChangeDirectoryIndicator)
             -> Command (ChangeDirectory (line.Substring(CommandChangeDirectoryIndicator.Length)))
            | line when line.StartsWith(CommandListIndicator)
             -> Command List
            | line when line.StartsWith(DirectoryIndicator)
             -> Directory (line.Substring(DirectoryIndicator.Length))
            | line
             -> File (parseFile line))
        |> List.ofSeq

    let private layoutStep layout line =
        match line with
        // Listing the current directory is a no-op
        | Command List
         -> layout
        | Command (ChangeDirectory relativePath) when relativePath = NavigateUp
         -> layout
            |> Layout.withCurrent (Layout.currentDirectory layout).Parent
        // Changing from the root to the root is a no-op
        | Command (ChangeDirectory relativePath) when relativePath = "/"
         -> layout
        | Command (ChangeDirectory relativePath)
         -> layout
            |> Layout.withCurrent (Layout.directoryNamed layout relativePath)
        | Directory name
         -> let newDir = Directory.empty name layout.Current
            let cur = Layout.currentDirectory layout
            let newIndex = layout.Directories.Length
            
            layout
            |> Layout.updatedCurrent (cur.withSub newIndex)
            |> Layout.withDirectory newDir
        | File file
         -> let cur = Layout.currentDirectory layout
            layout
            |> Layout.updatedCurrent (cur.withFile file)

    let layout (input: Line list) =
        input
        |> List.fold layoutStep Layout.empty
        |> Layout.withCurrent 0

    let directoriesWithMaxSize max (layout: Layout) =
        layout.Directories
        |> List.filter (fun dir -> (dir |> Directory.size layout) <= max)
