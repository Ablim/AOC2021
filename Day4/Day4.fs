module Day4

open System



let getPlaylist (rows: string list) =
    rows.Head.Split ','
    |> Array.map (fun x -> x |> int)
    |> Array.toList



let getBoards (rows: string list) =
    let mutable boardData = rows.Tail.Tail
    let mutable boards = []
    let mutable loop = true
    
    while loop do
        let board =
            seq {
                for _ in [0..4] do
                    let row =
                        boardData.Head.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                        |> Array.map (fun x -> (x |> int, false))
                    boardData <- boardData.Tail
                    yield row
                }
            |> Seq.toArray
        boards <- board :: boards

        if boardData = [] then
            loop <- false
        else
            boardData <- boardData.Tail
    boards



let tryMark (number: int) (board: (int * bool)[][]) =
    let mutable marked = false
    
    for i in [0..board.Length-1] do
        for j in [0..board.[0].Length-1] do
            let (num, mrkd) = board.[i][j]
            if number = num then
                board.[i][j] <- (num, true)
                marked <- true
    
    (board, marked)



let bingo (board: (int * bool)[][]) =
    let mutable result = false
    
    for i in [0..board.Length-1] do
        let row =
            seq {
                for j in [0..board.[0].Length-1] do
                    yield board.[i][j]
            }
        if row |> Seq.exists (fun (_, marked) -> marked = false) |> not then
            result <- true
    
    for i in [0..board.Length-1] do
        let row =
            seq {
                for j in [0..board.[0].Length-1] do
                    yield board.[j][i]
            }
        if row |> Seq.exists (fun (_, marked) -> marked = false) |> not then
            result <- true

    result



let sumUnmarked (board: (int * bool)[][]) =
    let mutable sum = 0

    for i in [0..board.Length-1] do
        for j in [0..board.[0].Length-1] do
            let (num, mrkd) = board.[i][j]
            if not mrkd then
                sum <- sum + num
    
    sum



let rec loopPlaylist (playlist: int list) (board: (int * bool)[][]) =
    match playlist with
    | [] -> (board, false, 0, 0)
    | h :: t ->
        let (newBoard, marked) = tryMark h board
            
        if (not marked) then
            loopPlaylist t board
        else
            if bingo newBoard then
                (newBoard, true, h, t.Length)
            else
                loopPlaylist t newBoard



let play (playlist: int list) (boards: (int * bool)[][] list) (best: bool) =
    let mutable bestBoard = boards.Head
    let mutable highscore = if best then 0 else Int32.MaxValue
    let mutable lastNumber = 0
    
    for board in boards do
        let (candidate, bingo, lastNo, score) = loopPlaylist playlist board
        
        if best && bingo && score > highscore then
            bestBoard <- candidate
            highscore <- score
            lastNumber <- lastNo
        elif best |> not && bingo && score < highscore then
            bestBoard <- candidate
            highscore <- score
            lastNumber <- lastNo
    
    sumUnmarked bestBoard * lastNumber



let solve (data: string list) =
    let playlist = getPlaylist data
    let boards = getBoards data
    play playlist boards true



let solve2 (data: string list) = 
    let playlist = getPlaylist data
    let boards = getBoards data
    play playlist boards false