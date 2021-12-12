namespace Test

module Day8Tests = 
    open Xunit
    open System.IO
    open Day8

    let filename = "Data/Day8Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve input
        Assert.Equal(26, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = solve2 input
        Assert.Equal(61229, result)

    [<Theory>]
    [<InlineDataAttribute("abcefg", 0)>]
    [<InlineDataAttribute("cf", 1)>]
    [<InlineDataAttribute("acdeg", 2)>]
    [<InlineDataAttribute("acdfg", 3)>]
    [<InlineDataAttribute("bcdf", 4)>]
    [<InlineDataAttribute("abdfg", 5)>]
    [<InlineDataAttribute("abdefg", 6)>]
    [<InlineDataAttribute("acf", 7)>]
    [<InlineDataAttribute("abcdefg", 8)>]
    [<InlineDataAttribute("abcdfg", 9)>]
    let ``Print sequence`` sequence expected =
        let result = displayPrint sequence
        Assert.Equal(expected, result.Value)
