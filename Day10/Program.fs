module Program

open System.IO
open Day10

[<EntryPoint>]
let main args =
    let input = File.ReadLines "Day10Part1.txt" |> Seq.toList
    let result = solve input
    printfn "Part 1: %i" result

    let result2 = solve2 input
    printfn "Part 2: %A" result2
    0