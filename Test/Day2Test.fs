namespace Test

module Day2Test = 
    open Xunit
    open Day2
    open System.IO

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines "Data/Day2Part1.txt" |> Seq.toList
        let result = solve input
        Assert.Equal(150, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines "Data/Day2Part1.txt" |> Seq.toList
        let result = solve2 input
        Assert.Equal(900, result)
