namespace Day13

module Solver =
    let createSheet (data: string list) =
        let splitIndex = data |> List.findIndex (fun x -> x = "")
        let (coordinates, folding) =
            data
            |> List.filter (fun x -> x = "" |> not)
            |> List.splitAt splitIndex 
        let separated = coordinates |> List.map (fun x ->
            let result = x.Split ','
            (result.[0] |> int, result.[1] |> int))
        let maxX =
            separated
            |> List.map (fun (x, _) -> x)
            |> List.max
        let maxY =
            separated
            |> List.map (fun (_, y) -> y)
            |> List.max
        let sheet = Array2D.zeroCreate<int> (maxY + 1) (maxX + 1)
        separated |> List.iter (fun (x, y) -> sheet.[y, x] <- 1)
        (sheet, folding)


    let foldUp (sheet: int[,]) (foldLine: int) =
        let width = sheet |> Array2D.length2
        let oldHeight = sheet |> Array2D.length1
        let newHeight = foldLine
        let folded = Array2D.zeroCreate<int> newHeight width

        for row in [0..oldHeight - 1] do
            for col in [0..width - 1] do
                if row < foldLine then
                    folded.[row, col] <- sheet.[row, col]
                elif row > foldLine then
                    let mirrorRow = foldLine - (row - foldLine)
                    folded.[mirrorRow, col] <- max folded.[mirrorRow, col] sheet.[row, col]
                    
        folded


    let foldLeft (sheet: int[,]) (foldLine: int) =
        let oldWidth = sheet |> Array2D.length2
        let newWidth = foldLine
        let height = sheet |> Array2D.length1
        let folded = Array2D.zeroCreate<int> height newWidth

        for row in [0..height - 1] do
            for col in [0..oldWidth - 1] do
                if col < foldLine then
                    folded.[row, col] <- sheet.[row, col]
                elif col > foldLine then
                    let mirrorCol = foldLine - (col - foldLine)
                    folded.[row, mirrorCol] <- max folded.[row, mirrorCol] sheet.[row, col]
                    
        folded


    let fold (sheet: int[,]) (instruction: string) =
        let parts = (instruction.Split ' ').[2].Split '='
        let (direction, position) = (parts.[0], parts.[1] |> int)
        
        match direction with
        | "x" -> foldLeft sheet position
        | _ -> foldUp sheet position
    

    let rec foldLoop (instructions: string list) (sheet: int[,]) =
        match instructions with
        | [] -> sheet
        | next :: tail -> foldLoop tail (fold sheet next)

    let solve (data: string list) =
        let (sheet, folding) = createSheet data
        let mutable points = 0

        let foldOnce = fold sheet folding.Head
        foldOnce |> Array2D.iter (fun x -> if x > 0 then points <- points + 1)
        points


    let solve2 (data: string list) =
        let (sheet, folding) = createSheet data
        let folded = foldLoop folding sheet
        folded