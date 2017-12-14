// https://adventofcode.com/2017/day/14

let l = [|0..255|]
let ll = Array.length l

// HACK: horrific "circular" array reversing code I could understand, thanks to
// pay.reddit.com/r/adventofcode/comments/7irzg5/2017_day_10_solutions/dr11jzy/
let tie (list: int[]) cur length =
    let subList = Array.create<int> length 0
    for i = 0 to length - 1 do
        subList.[i] <- list.[(cur + i) % ll]
    let revSubList = Array.rev subList
    for i = 0 to length - 1 do
        list.[(cur + i) % ll] <- revSubList.[i]
    list

let rec run (list: int[]) cur skip lengths =
    if List.length lengths = 0 then
        (list, cur, skip)
    else 
        let length = List.head lengths
        let newList = tie (Array.copy list) cur length
        let newSkip = skip + 1
        let newCur = (cur + length + skip) % ll
        run newList newCur newSkip (List.tail lengths)

let run2 list lengths =
    let appendedInput =
        List.append lengths [ 17; 31; 73; 47; 23 ]
    // get part 1 a few times
    let rec run2internal list cur skip input rounds = 
        let (newList, newCur, newSkip) = run list cur skip input
        if rounds = 0 then
            newList
        else
            run2internal newList newCur newSkip input (rounds - 1)
    let sparseHash = run2internal list 0 0 appendedInput 63
    let denseHash = sparseHash |> Array.chunkBySize 16
    let denseProcessed =
        Array.map (fun x ->
            Array.fold (fun a e ->
                a ^^^ e) (Array.head x) (Array.tail x)) denseHash
    let denseFormatted =
        Array.fold (fun a e -> a + Printf.sprintf "%02x" e) "" denseProcessed
    denseFormatted

let str2byte s = 
    Seq.cast<char> s |> Seq.map (fun x -> int x) |> Seq.toList

let binaryString (s: string) =
    s
        |> Seq.map (fun x -> "0x" + string x)
        |> Seq.map byte
        |> Seq.map (fun x ->
            System.Convert.ToString(x, 2).PadLeft(4, '0'))
        |> System.String.Concat

let rec floodFill (y, x) toReplace replacement (array: int[,]) =
    let inBounds =
        (y >= 0 && y < Array2D.length1 array) &&
        (x >= 0 && x < Array2D.length2 array)
    if inBounds && array.[y, x] = toReplace then
        array.[y, x] <- replacement
        floodFill (y - 1, x) toReplace replacement array
        floodFill (y + 1, x) toReplace replacement array
        floodFill (y, x - 1) toReplace replacement array
        floodFill (y, x + 1) toReplace replacement array
        0 |> ignore
    else
        0 |> ignore

let array2Dmax a = a |> Seq.cast<int> |> Seq.max

[<EntryPoint>]
let main argv = 
    let input = "ugkiagan"
    let processedInputs =
        [0..127]
            |> List.map (fun x -> input + "-" + string x)
            |> List.map str2byte
            |> List.map (run2 l)
            |> List.map binaryString
    let part1res =
        processedInputs
            |> List.fold (fun a e -> a + e) ""
            |> Seq.countBy (fun x -> x = '1')
            |> Seq.filter fst
            |> Seq.map snd
            |> Seq.exactlyOne
    let part2array = Array2D.create 128 128 0
    for i in [0..127] do
        for j in [0..127] do
            part2array.[j, i] <-
                List.item j processedInputs
                    |> Seq.item i
                    |> string
                    |> int
    
    for i in [0..127] do
        for j in [0..127] do
            let nextHigh = (array2Dmax part2array) + 1
            floodFill (j, i) 1 nextHigh part2array
    let part2res = (array2Dmax part2array) - 1
    printfn "Part 1: %d; Part 2: %d" part1res part2res
    assert(false)
    0 // return an integer exit code
