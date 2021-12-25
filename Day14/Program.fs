namespace Day14

module Program =
    open System.IO

    [<EntryPoint>]
    let main args =        
        let input = File.ReadLines "Day14Part1.txt" |> Seq.toList
        let result = Solver.solve input
        printfn "Part 1: %i" result

        let result2 = Solver.solve2 input
        printfn "Part 2: %i" result2
        0