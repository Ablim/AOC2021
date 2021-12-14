namespace Test

module Day1Test = 
    open Xunit
    open Day1
    open System.IO

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines "Data/Day1Part1.txt" |> Seq.toList
        let result = solve input
        Assert.Equal(7, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines "Data/Day1Part1.txt" |> Seq.toList
        let result = solve2 input
        Assert.Equal(5, result)
