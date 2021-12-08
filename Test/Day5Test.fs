namespace Test

module Day5Tests = 
    open Xunit
    open System.IO
    open Day5

    [<Fact>]
    let ``Day 5 Part 1`` () =
        let input = File.ReadLines "Data/Day5Part1.txt" |> Seq.toList
        let result = solve input
        Assert.Equal(5, result)

    [<Fact>]
    let ``Day 5 Part 2`` () =
        let input = File.ReadLines "Data/Day5Part1.txt" |> Seq.toList
        let result = solve2 input
        Assert.Equal(12, result)
