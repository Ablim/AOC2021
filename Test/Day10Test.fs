namespace Test

module Day10Test = 
    open Xunit
    open System.IO
    open Day10

    let filename = "Data/Day10Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve input
        Assert.Equal(26397, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve2 input
        Assert.Equal(288957I, result)
