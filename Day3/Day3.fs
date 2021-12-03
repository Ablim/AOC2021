module Day3

let getBits (sequence: string) =
    sequence.ToCharArray()
    |> Seq.map (fun x -> x |> string)
    |> Seq.map (fun x -> x |> int)
    |> Seq.toArray

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

    let gamma =
        sums
        |> Seq.map (toBit (rows / 2))
        |> Seq.toArray
    let epsilon =
        gamma
        |> Seq.map (fun x -> x - 1 |> abs)
        |> Seq.toArray

    (toDecimal gamma) * (toDecimal epsilon)

let rec search (position: int) = 
    0

let solve2 (data: List<string>) =
    let bitList = Seq.map getBits data
    0