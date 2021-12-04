﻿module Program

open System.IO
open Day3

[<EntryPoint>]
let main args =
    let input = File.ReadLines "Day3Part1.txt" |> Seq.toList
    let result = solve input
    printfn "Part 1: %i" result

    let result2 = solve2 input
    printfn "Part 2: %i" result2
    0