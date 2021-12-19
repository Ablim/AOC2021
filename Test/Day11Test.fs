namespace Test

module Day11Test = 
    open Xunit
    open System.IO
    open Day11

    let filename = "Data/Day11Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve input
        Assert.Equal(1656, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve2 input
        Assert.Equal(195, result)
