module SegmentUtils

let canPrint2 (display: char[]) (sequence: string)  =
    let two = [
        display.[0];
        display.[2];
        display.[3];
        display.[4];
        display.[6]
    ]
    two |> List.forall (fun x -> sequence.Contains(x))

let canPrint3 (display: char[]) (sequence: string)  =
    let three = [
        display.[0];
        display.[2];
        display.[3];
        display.[5];
        display.[6]
    ]
    three |> List.forall (fun x -> sequence.Contains(x))

let canPrint5 (display: char[]) (sequence: string)  =
    let five = [
        display.[0];
        display.[1];
        display.[3];
        display.[5];
        display.[6]
    ]
    five |> List.forall (fun x -> sequence.Contains(x))

let canPrint0 (display: char[]) (sequence: string)  =
    let zero = [
        display.[0];
        display.[1];
        display.[2];
        display.[4];
        display.[5];
        display.[6]
    ]
    zero |> List.forall (fun x -> sequence.Contains(x))

let canPrint6 (display: char[]) (sequence: string)  =
    let six = [
        display.[0];
        display.[1];
        display.[3];
        display.[4];
        display.[5];
        display.[6]
    ]
    six |> List.forall (fun x -> sequence.Contains(x))

let canPrint9 (display: char[]) (sequence: string)  =
    let nine = [
        display.[0];
        display.[1];
        display.[2];
        display.[3];
        display.[5];
        display.[6]
    ]
    nine |> List.forall (fun x -> sequence.Contains(x))