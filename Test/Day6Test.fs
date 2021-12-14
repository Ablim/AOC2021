namespace Test

module Day6Test = 
    open Xunit
    open System.IO
    open Day6

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines "Data/Day6Part1.txt" |> Seq.toList
        let result = solve input
        Assert.Equal(5934, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines "Data/Day6Part1.txt" |> Seq.toList
        let result = solve2 input 80
        Assert.Equal(5934I, result)
