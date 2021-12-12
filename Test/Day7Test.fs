namespace Test

module Day7Tests = 
    open Xunit
    open System.IO
    open Day7

    let filename = "Data/Day7Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve input
        Assert.Equal(37, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve2 input
        Assert.Equal(168, result)
