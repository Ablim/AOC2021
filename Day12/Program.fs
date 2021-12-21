namespace Day12

module Program =
    open System.IO

    [<EntryPoint>]
    let main args =
        
        let lala = [ 1; 1; 1; 2; 2; 3 ]
        let lala2 = lala |> List.groupBy (fun x -> x)
        
        
        let input = File.ReadLines "Day12Part1.txt" |> Seq.toList
        let result = Solver.solve input
        printfn "Part 1: %i" result

        let result2 = Solver.solve2 input
        printfn "Part 2: %i" result2
        0