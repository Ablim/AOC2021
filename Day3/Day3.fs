module Day3

let getBits (sequence: string) =
    sequence.ToCharArray()
    |> Array.map (fun x -> x |> string)
    |> Array.map (fun x -> x |> int)

let toBit middle number =
    if number > middle then 1 else 0

let toDecimal (bits: int[]) =
    let positions = [0..(bits.Length - 1)]
    bits
    |> Seq.rev
    |> Seq.zip positions
    |> Seq.map (fun (pos, bit) -> bit * pown 2 pos)
    |> Seq.sum

let solve (data: List<string>) =
    let bitCount = data.Head.Length
    let rows = data.Length
    let sums: int[] = Array.zeroCreate bitCount

    for row in data do
        let bits = getBits row

        for i in 0..(bitCount - 1) do
            sums.[i] <- sums.[i] + bits.[i]

    let gamma = Array.map (toBit (rows / 2)) sums
    let epsilon = Array.map (fun x -> x - 1 |> abs) gamma
    (toDecimal gamma) * (toDecimal epsilon)

let giveList (zeros: List<'a>) (ones: List<'a>) (getLargest: bool) =
    if zeros.Length = ones.Length then
        if getLargest then ones else zeros
    else
        let (large, small) = if zeros.Length > ones.Length then (zeros, ones) else (ones, zeros)
        if getLargest then large else small

let rec search (numbers: List<int[]>) (position: int) (mostCommon: bool) = 
    if numbers.Length = 1 then
        numbers.Head
    else
        let mutable zeroList = []
        let mutable oneList = []
    
        numbers |> List.iter (fun n ->
            if n.[position] = 0 then zeroList <- n :: zeroList
            else oneList <- n :: oneList)

        let nextNumbers = giveList zeroList oneList mostCommon
        search nextNumbers (position + 1) mostCommon

let solve2 (data: List<string>) =
    let numbers = List.map getBits data
    let o2 = search numbers 0 true
    let co2 = search numbers 0 false
    (toDecimal o2) * (toDecimal co2)