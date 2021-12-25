namespace Day13

module Program =
    open System.IO
    open Utils

    [<EntryPoint>]
    let main args =        
        let input = File.ReadLines "Day13Part1.txt" |> Seq.toList
        let result = Solver.solve input
        printfn "Part 1: %i" result

        let result2 = Solver.solve2 input
        printfn "Part 2:"
        Printer.print2D result2
        0