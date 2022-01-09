namespace Day16

module Program =
    open System.IO

    [<EntryPoint>]
    let main args =        
        let input = File.ReadLines "Day16Part1.txt" |> Seq.head
        let result = Solver.solve input
        printfn "Part 1: %i" result

        let result2 = Solver.solve2 input
        printfn "Part 2: %A" result2
        0