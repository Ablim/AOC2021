namespace Test

module Day9Test = 
    open Xunit
    open System.IO
    open Day9

    let filename = "Data/Day9Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve input
        Assert.Equal(15, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve2 input
        Assert.Equal(1134, result)
