namespace Test

module Day3Test = 
    open Xunit
    open System.IO
    open Day3

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines "Data/Day3Part1.txt" |> Seq.toList
        let result = solve input
        Assert.Equal(198, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines "Data/Day3Part1.txt" |> Seq.toList
        let result = solve2 input
        Assert.Equal(230, result)
