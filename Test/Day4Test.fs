namespace Test

module Day4Tests =
    open Xunit
    open System.IO
    open Day4

    [<Fact>]
    let ``Day 4 Part 1`` () =
        let input = File.ReadLines "Data/Day4Part1.txt" |> Seq.toList
        let result = solve input
        Assert.Equal(4512, result)

    [<Fact>]
    let ``Day 4 Part 2`` () =
        let input = File.ReadLines "Data/Day4Part1.txt" |> Seq.toList
        let result = solve2 input
        Assert.Equal(1924, result)
