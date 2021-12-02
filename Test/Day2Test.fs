module Day2Tests

open Xunit
open Day2
open System.IO

[<Fact>]
let ``Day 2 Part 1`` () =
    let input = File.ReadLines "Data/Day2Part1.txt" |> Seq.toList
    let result = solve input
    Assert.Equal(150, result)

[<Fact>]
let ``Day 2 Part 2`` () =
    let input = File.ReadLines "Data/Day2Part1.txt" |> Seq.toList
    let result = solve2 input
    Assert.Equal(900, result)
